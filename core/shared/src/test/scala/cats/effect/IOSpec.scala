/*
 * Copyright 2020 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.effect

import cats.{Eq, Order, Show}
import cats.kernel.laws.discipline.MonoidTests
import cats.effect.laws.EffectTests
import cats.effect.testkit.{AsyncGenerators, BracketGenerators, GenK, OutcomeGenerators, TestContext}
import cats.implicits._

import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}, Prop.forAll
// import org.scalacheck.rng.Seed

import org.specs2.ScalaCheck
// import org.specs2.scalacheck.Parameters
import org.specs2.matcher.Matcher

import org.typelevel.discipline.specs2.mutable.Discipline

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import java.util.concurrent.TimeUnit

class IOSpec extends IOPlatformSpecification with Discipline with ScalaCheck { outer =>
  import OutcomeGenerators._

  sequential

  val ctx = TestContext()

  "io monad" should {
    "produce a pure value when run" in {
      IO.pure(42) must completeAs(42)
    }

    "suspend a side-effect without memoizing" in {
      var i = 42

      val ioa = IO {
        i += 1
        i
      }

      ioa must completeAs(43)
      ioa must completeAs(44)
    }

    "capture errors in suspensions" in {
      case object TestException extends RuntimeException
      IO(throw TestException) must failAs(TestException)
    }

    "resume value continuation within async" in {
      IO.async[Int](k => IO(k(Right(42))).map(_ => None)) must completeAs(42)
    }

    "resume error continuation within async" in {
      case object TestException extends RuntimeException
      IO.async[Unit](k => IO(k(Left(TestException))).as(None)) must failAs(TestException)
    }

    "map results to a new type" in {
      IO.pure(42).map(_.toString) must completeAs("42")
    }

    "flatMap results sequencing both effects" in {
      var i = 0
      IO.pure(42).flatMap(i2 => IO { i = i2 }) must completeAs(())
      i mustEqual 42
    }

    "raiseError propagates out" in {
      case object TestException extends RuntimeException
      IO.raiseError(TestException).void.flatMap(_ => IO.pure(())) must failAs(TestException)
    }

    "errors can be handled" in {
      case object TestException extends RuntimeException
      IO.raiseError[Unit](TestException).attempt must completeAs(Left(TestException))
    }

    "start and join on a successful fiber" in {
      IO.pure(42).map(_ + 1).start.flatMap(_.join) must completeAs(Outcome.completed[IO, Throwable, Int](IO.pure(43)))
    }

    "start and join on a failed fiber" in {
      case object TestException extends RuntimeException
      (IO.raiseError(TestException): IO[Unit]).start.flatMap(_.join) must completeAs(Outcome.errored[IO, Throwable, Unit](TestException))
    }

    "implement never with non-terminating semantics" in {
      IO.never must nonTerminate
    }

    "start and ignore a non-terminating fiber" in {
      IO.never.start.as(42) must completeAs(42)
    }

    "start a fiber then continue with its results" in {
      IO.pure(42).start.flatMap(_.join) flatMap { oc =>
        oc.fold(IO.pure(0), _ => IO.pure(-1), ioa => ioa)
      } must completeAs(42)
    }

    "continue from the results of an async produced prior to registration" in {
      IO.async[Int](cb => IO(cb(Right(42))).as(None)).map(_ + 2) must completeAs(44)
    }

    "produce a failure when the registration raises an error after callback" in {
      case object TestException extends RuntimeException
      IO.async[Int](cb => IO(cb(Right(42))).flatMap(_ => IO.raiseError(TestException))).void must failAs(TestException)
    }

    "cancel an infinite chain of right-binds" in {
      lazy val infinite: IO[Unit] = IO.unit.flatMap(_ => infinite)
      infinite.start.flatMap(f => f.cancel >> f.join) must completeAs(Outcome.canceled[IO, Throwable, Unit])
    }

    "cancel never" in {
      (IO.never: IO[Unit]).start.flatMap(f => f.cancel >> f.join) must completeAs(Outcome.canceled[IO, Throwable, Unit])
    }

    "cancel never after scheduling" in {
      val ioa = for {
        f <- (IO.never: IO[Unit]).start
        ec <- IO.executionContext
        _ <- IO(ec.asInstanceOf[TestContext].tickAll())
        _ <- f.cancel
        oc <- f.join
      } yield oc

      ioa must completeAs(Outcome.canceled[IO, Throwable, Unit])
    }

    "sequence async cancel token upon cancelation during suspension" in {
      var affected = false

      val target = IO.async[Unit] { _ =>
        IO.pure(Some(IO { affected = true }))
      }

      val ioa = for {
        f <- target.start
        _ <- IO(ctx.tickAll())
        _ <- f.cancel
      } yield ()

      ioa must completeAs(())
      affected must beTrue
    }

    "suppress async cancel token upon cancelation in masked region" in {
      var affected = false

      val target = IO uncancelable { _ =>
        IO.async[Unit] { _ =>
          IO.pure(Some(IO { affected = true }))
        }
      }

      val ioa = for {
        f <- target.start
        _ <- IO(ctx.tickAll())
        _ <- f.cancel
      } yield ()

      ioa must completeAs(())
      affected must beFalse
    }

    "preserve contexts through start" in {
      val ec = ctx.derive()

      val ioa = for {
        f <- IO.executionContext.start.evalOn(ec)
        _ <- IO(ctx.tickAll())
        oc <- f.join
      } yield oc

      ioa must completeAs(Outcome.completed[IO, Throwable, ExecutionContext](IO.pure(ec)))
    }

    "preserve monad identity on async" in {
      val fa = IO.async[Int](cb => IO(cb(Right(42))).as(None))
      fa.flatMap(i => IO.pure(i)) must completeAs(42)
      fa must completeAs(42)
    }

    "preserve monad right identity on uncancelable" in {
      val fa = IO.uncancelable(_ => IO.canceled)
      fa.flatMap(IO.pure(_)) must nonTerminate
      fa must nonTerminate
    }

    "cancel flatMap continuations following a canceled uncancelable block" in {
      IO.uncancelable(_ => IO.canceled).flatMap(_ => IO.pure(())) must nonTerminate
    }

    "cancel map continuations following a canceled uncancelable block" in {
      IO.uncancelable(_ => IO.canceled).map(_ => ()) must nonTerminate
    }

    "mapping something with a finalizer should complete" in {
      IO.pure(42).onCancel(IO.unit).as(()) must completeAs(())
    }

    "uncancelable canceled with finalizer within fiber should not block" in {
      val fab = IO.uncancelable(_ => IO.canceled.onCancel(IO.unit)).start.flatMap(_.join)

      fab must completeAs(Outcome.canceled[IO, Throwable, Unit])
    }

    "uncancelable canceled with finalizer within fiber should flatMap another day" in {
      val fa = IO.pure(42)
      val fab: IO[Int => Int] =
        IO.uncancelable(_ => IO.canceled.onCancel(IO.unit)).start.flatMap(_.join).flatMap(_ => IO.pure((i: Int) => i))

      fab.ap(fa) must completeAs(42)
      fab.flatMap(f => fa.map(f)) must completeAs(42)
    }

    "sleep for ten seconds" in {
      IO.sleep(10.seconds).as(1) must completeAs(1)
    }

    "sleep for ten seconds and continue" in {
      var affected = false
      (IO.sleep(10.seconds) >> IO { affected = true }) must completeAs(())
      affected must beTrue
    }

    "run an identity finalizer" in {
      var affected = false

      IO.unit onCase {
        case _ => IO { affected = true }
      } must completeAs(())

      affected must beTrue
    }

    "run an identity finalizer and continue" in {
      var affected = false

      val seed = IO.unit onCase {
        case _ => IO { affected = true }
      }

      seed.as(42) must completeAs(42)

      affected must beTrue
    }

    "run multiple nested finalizers on cancel" in {
      var inner = false
      var outer = false

      IO.canceled.guarantee(IO { inner = true }).guarantee(IO { outer = true }) must nonTerminate

      inner must beTrue
      outer must beTrue
    }

    "run multiple nested finalizers on completion exactly once" in {
      var inner = 0
      var outer = 0

      IO.unit.guarantee(IO { inner += 1 }).guarantee(IO { outer += 1 }) must completeAs(())

      inner mustEqual 1
      outer mustEqual 1
    }

    "sequence onCancel when canceled before registration" in {
      var passed = false
      val test = IO uncancelable { poll =>
        IO.canceled >> poll(IO.unit).onCancel(IO { passed = true })
      }

      test must nonTerminate
      passed must beTrue
    }

    "break out of uncancelable when canceled before poll" in {
      var passed = true
      val test = IO uncancelable { poll =>
        IO.canceled >> poll(IO.unit) >> IO { passed = false }
      }

      test must nonTerminate
      passed must beTrue
    }

    "invoke onCase finalizer when cancelable async returns" in {
      var passed = false

      // convenient proxy for an async that returns a cancelToken
      val test = IO.sleep(1.day) onCase {
        case Outcome.Completed(_) => IO { passed = true }
      }

      test must completeAs(())
      passed must beTrue
    }

    "hold onto errors through multiple finalizers" in {
      case object TestException extends RuntimeException
      IO.raiseError(TestException).guarantee(IO.unit).guarantee(IO.unit) must failAs(TestException)
    }

    "cede unit in a finalizer" in {
      val body = IO.sleep(1.second).start.flatMap(_.join).map(_ => 42)
      body.guarantee(IO.cede.map(_ => ())) must completeAs(42)
    }

    "not invoke onCancel when previously canceled within uncancelable" in {
      var failed = false
      IO.uncancelable(_ => IO.canceled >> IO.unit.onCancel(IO { failed = true })) must nonTerminate
      failed must beFalse
    }

    "complete a fiber with Canceled under finalizer on poll" in {
      val ioa = IO.uncancelable(p => IO.canceled >> p(IO.unit).guarantee(IO.unit)).start.flatMap(_.join)

      ioa must completeAs(Outcome.canceled[IO, Throwable, Unit])
    }

    "return the left when racing against never" in {
      IO.pure(42).racePair(IO.never: IO[Unit]).map(_.left.toOption.map(_._1)) must completeAs(Some(42))
    }

    "produce Canceled from start of canceled" in {
      IO.canceled.start.flatMap(_.join) must completeAs(Outcome.canceled[IO, Throwable, Unit])
    }

    "cancel an already canceled fiber" in {
      val test = for {
        f <- IO.canceled.start
        _ <- IO(ctx.tickAll())
        _ <- f.cancel
      } yield ()

      test must completeAs(())
    }

    "only unmask within current fiber" in {
      var passed = false
      val test = IO uncancelable { poll =>
        IO.uncancelable(_ => poll(IO.canceled >> IO { passed = true })).start.flatMap(_.join).void
      }

      test must completeAs(())
      passed must beTrue
    }

    "produce the left when the right errors in racePair" in {
      (IO.cede >> IO.pure(42)).racePair(IO.raiseError(new Throwable): IO[Unit]).map(_.left.toOption.map(_._1)) must completeAs(Some(42))
    }

    "run three finalizers when an async is canceled while suspended" in {
      var results = List[Int]()

      val body = IO.async[Nothing] { _ =>
        IO.pure(Some(IO(results ::= 3)))
      }

      val test = for {
        f <- body.onCancel(IO(results ::= 2)).onCancel(IO(results ::= 1)).start
        _ <- IO(ctx.tickAll())
        _ <- f.cancel
        back <- IO(results)
      } yield back

      test must completeAs(List(1, 2, 3))
    }

    "evaluate 10,000 consecutive map continuations" in {
      def loop(i: Int): IO[Unit] =
        if (i < 10000)
          IO.unit.flatMap(_ => loop(i + 1)).map(u => u)
        else
          IO.unit

      loop(0) must completeAs(())
    }

    "evaluate 10,000 consecutive handleErrorWith continuations" in {
      def loop(i: Int): IO[Unit] =
        if (i < 10000)
          IO.unit.flatMap(_ => loop(i + 1)).handleErrorWith(IO.raiseError(_))
        else
          IO.unit

      loop(0) must completeAs(())
    }

    "catch exceptions thrown in map functions" in {
      case object TestException extends RuntimeException
      IO.unit.map(_ => (throw TestException): Unit).attempt must completeAs(Left(TestException))
    }

    "catch exceptions thrown in flatMap functions" in {
      case object TestException extends RuntimeException
      IO.unit.flatMap(_ => (throw TestException): IO[Unit]).attempt must completeAs(Left(TestException))
    }

    "catch exceptions thrown in handleErrorWith functions" in {
      case object TestException extends RuntimeException
      case object WrongException extends RuntimeException
      IO.raiseError[Unit](WrongException).handleErrorWith(_ => (throw TestException): IO[Unit]).attempt must completeAs(Left(TestException))
    }

    "round trip through s.c.Future" in forAll { (ioa: IO[Int]) =>
      ioa eqv IO.fromFuture(IO(IORuntime(ctx, timer()).unsafeToFuture(ioa)))
    }

    platformSpecs
  }

  {
    checkAll(
      "IO",
      EffectTests[IO].effect[Int, Int, Int](10.millis))/*(Parameters(seed = Some(Seed.fromBase64("XidlR_tu11X7_v51XojzZJsm6EaeU99RAEL9vzbkWBD=").get)))*/

    checkAll(
      "IO[Int]",
      MonoidTests[IO[Int]].monoid)/*(Parameters(seed = Some(Seed.fromBase64("_1deH2u9O-z6PmkYMBgZT-3ofsMEAMStR9x0jKlFgyO=").get)))*/
  }

  // TODO organize the below somewhat better

  implicit def cogenIO[A: Cogen]: Cogen[IO[A]] =
    Cogen[Outcome[Option, Throwable, A]].contramap(unsafeRun(_))

  implicit def arbitraryIO[A: Arbitrary: Cogen]: Arbitrary[IO[A]] = {
    val generators =
      new AsyncGenerators[IO] with BracketGenerators[IO, Throwable] {

        val arbitraryE: Arbitrary[Throwable] =
          arbitraryThrowable

        val cogenE: Cogen[Throwable] = Cogen[Throwable]

        val F: AsyncBracket[IO] = IO.effectForIO

        def cogenCase[B: Cogen]: Cogen[Outcome[IO, Throwable, B]] =
          OutcomeGenerators.cogenOutcome[IO, Throwable, B]

        val arbitraryEC: Arbitrary[ExecutionContext] = outer.arbitraryEC

        val cogenFU: Cogen[IO[Unit]] = cogenIO[Unit]

        // TODO dedup with FreeSyncGenerators
        val arbitraryFD: Arbitrary[FiniteDuration] = outer.arbitraryFD

        override def recursiveGen[B: Arbitrary: Cogen](deeper: GenK[IO]) =
          super.recursiveGen[B](deeper).filterNot(_._1 == "racePair")   // remove the racePair generator since it reifies nondeterminism, which cannot be law-tested
      }

    Arbitrary(generators.generators[A])
  }

  implicit lazy val arbitraryFD: Arbitrary[FiniteDuration] = {
    import TimeUnit._

    val genTU = Gen.oneOf(NANOSECONDS, MICROSECONDS, MILLISECONDS, SECONDS, MINUTES, HOURS)

    Arbitrary {
      genTU flatMap { u =>
        Gen.choose[Long](0L, 48L).map(FiniteDuration(_, u))
      }
    }
  }

  implicit lazy val arbitraryThrowable: Arbitrary[Throwable] =
    Arbitrary(Arbitrary.arbitrary[Int].map(TestException))

  implicit lazy val arbitraryEC: Arbitrary[ExecutionContext] =
    Arbitrary(Gen.const(ctx.derive()))

  implicit lazy val eqThrowable: Eq[Throwable] =
    Eq.fromUniversalEquals[Throwable]

  implicit lazy val shThrowable: Show[Throwable] =
    Show.fromToString[Throwable]

  implicit lazy val eqEC: Eq[ExecutionContext] =
    Eq.fromUniversalEquals[ExecutionContext]

  implicit lazy val ordIOFD: Order[IO[FiniteDuration]] =
    Order by { ioa =>
      unsafeRun(ioa).fold(
        None,
        _ => None,
        fa => fa)
    }

  implicit def eqIOA[A: Eq]: Eq[IO[A]] = {
    /*Eq instance { (left: IO[A], right: IO[A]) =>
      val leftR = unsafeRun(left)
      val rightR = unsafeRun(right)

      val back = leftR eqv rightR

      if (!back) {
        println(s"$left != $right")
        println(s"$leftR != $rightR")
      }

      back
    }*/

    Eq.by(unsafeRun(_))
  }

  // feel the rhythm, feel the rhyme...
  implicit def boolRunnings(iob: IO[Boolean]): Prop =
    Prop(unsafeRun(iob).fold(false, _ => false, _.getOrElse(false)))

  def completeAs[A: Eq: Show](expected: A): Matcher[IO[A]] =
    tickTo(Outcome.Completed(Some(expected)))

  def failAs(expected: Throwable): Matcher[IO[Unit]] =
    tickTo[Unit](Outcome.Errored(expected))

  def nonTerminate: Matcher[IO[Unit]] =
    tickTo[Unit](Outcome.Completed(None))

  def tickTo[A: Eq: Show](expected: Outcome[Option, Throwable, A]): Matcher[IO[A]] = { (ioa: IO[A]) =>
    val oc = unsafeRun(ioa)
    (oc eqv expected, s"${oc.show} !== ${expected.show}")
  }

  def unsafeRun[A](ioa: IO[A]): Outcome[Option, Throwable, A] = {
    try {
      var results: Outcome[Option, Throwable, A] = Outcome.Completed(None)

      IORuntime(ctx, timer()).unsafeRunAsync(ioa) {
        case Left(t) => results = Outcome.Errored(t)
        case Right(a) => results = Outcome.Completed(Some(a))
      }

      ctx.tickAll(3.days)

      /*println("====================================")
      println(s"completed ioa with $results")
      println("====================================")*/

      results
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }
  }

  def timer(): UnsafeTimer =
    new UnsafeTimer {
      def sleep(delay: FiniteDuration, action: Runnable): Runnable = {
        val cancel = ctx.schedule(delay, action)
        new Runnable { def run() = cancel() }
      }

      def nowMillis() = ctx.now().toMillis
      def monotonicNanos() = ctx.now().toNanos
    }
}

final case class TestException(i: Int) extends RuntimeException
