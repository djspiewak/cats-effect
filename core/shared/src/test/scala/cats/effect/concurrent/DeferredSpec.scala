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

package cats
package effect
package concurrent

import cats.{Eq, Order, Show}
import cats.data.State
import cats.kernel.laws.discipline.MonoidTests
import cats.effect.laws.EffectTests
import cats.effect.testkit.{AsyncGenerators, BracketGenerators, GenK, OutcomeGenerators, TestContext}
import cats.implicits._

import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
// import org.scalacheck.rng.Seed

import org.specs2.ScalaCheck
// import org.specs2.scalacheck.Parameters
import org.specs2.matcher.Matcher
import org.specs2.specification.core.Fragments

import org.typelevel.discipline.specs2.mutable.Discipline

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import java.util.concurrent.TimeUnit

class DeferresSpec extends IOPlatformSpecification with Discipline with ScalaCheck with BaseSpec { outer =>

  import OutcomeGenerators._

  sequential

  val ctx = TestContext()

  trait DeferredConstructor { def apply[A]: IO[Deferred[IO, A]] }
  trait TryableDeferredConstructor { def apply[A]: IO[TryableDeferred[IO, A]] }

  "deferred" should {

    //TODO does this distinction make sense any more?
    tests("concurrent", new DeferredConstructor { def apply[A] = Deferred[IO, A] })
    tests("concurrentTryable", new DeferredConstructor { def apply[A] = Deferred.tryable[IO, A] })
    tests("async", new DeferredConstructor { def apply[A] = Deferred.uncancelable[IO, A] })
    tests("asyncTryable", new DeferredConstructor { def apply[A] = Deferred.tryableUncancelable[IO, A] })

    tryableTests("concurrentTryable", new TryableDeferredConstructor { def apply[A] = Deferred.tryable[IO, A] })
    tryableTests("asyncTryable", new TryableDeferredConstructor { def apply[A] = Deferred.tryableUncancelable[IO, A] })

    "concurrent - get - cancel before forcing" in {
      cancelBeforeForcing(Deferred.apply) must completeAs(None)
    }

  }

  def tests(label: String, pc: DeferredConstructor): Fragments = {
    s"$label - complete" in {
      pc[Int]
        .flatMap { p =>
          p.complete(0) *> p.get
        } must completeAs(0)
    }

//     test(s"$label - complete is only successful once") {
//       pc[Int]
//         .flatMap { p =>
//           (p.complete(0) *> p.complete(1).attempt).product(p.get)
//         }
//         .unsafeToFuture()
//         .map {
//           case (err, value) =>
//             err.swap.toOption.get shouldBe an[IllegalStateException]
//             value shouldBe 0
//         }
//     }

//     test(s"$label - complete is only successful once") {
//       pc[Int]
//         .flatMap { p =>
//           (p.complete(0) *> p.complete(1).attempt).product(p.get)
//         }
//         .unsafeToFuture()
//         .map {
//           case (err, value) =>
//             err.swap.toOption.get shouldBe an[IllegalStateException]
//             value shouldBe 0
//         }
//     }

    s"$label - get blocks until set" in {
      val op = for {
        state <- Ref[IO].of(0)
        modifyGate <- pc[Unit]
        readGate <- pc[Unit]
        _ <- (modifyGate.get *> state.update(_ * 2) *> readGate.complete(())).start
        _ <- (state.set(1) *> modifyGate.complete(())).start
        _ <- readGate.get
        res <- state.get
      } yield res

      op must completeAs(2)
    }
  }

  def tryableTests(label: String, pc: TryableDeferredConstructor): Fragments = {
    s"$label - tryGet returns None for unset Deferred" in {
      pc[Unit].flatMap(_.tryGet) must completeAs(None)
    }

    s"$label - tryGet returns Some() for set Deferred" in {
      val op = for {
        d <- pc[Unit]
        _ <- d.complete(())
        result <- d.tryGet
      } yield result

      op must completeAs(Some(()))
    }
  }

  private def cancelBeforeForcing(pc: IO[Deferred[IO, Int]]): IO[Option[Int]] =
    for {
      r <- Ref[IO].of(Option.empty[Int])
      p <- pc
      fiber <- p.get.start
      _ <- fiber.cancel
      _ <- (fiber.join.flatMap(i => r.set(Some(i)))).start
      _ <- IO.sleep(100.millis)
      _ <- p.complete(42)
      _ <- IO.sleep(100.millis)
      result <- r.get
    } yield result


//   test("issue #380: complete doesn't block, test #1") {
//     def execute(times: Int): IO[Assertion] = {
//       def foreverAsync(i: Int): IO[Unit] =
//         if (i == 512) IO.async[Unit](cb => cb(Right(()))) >> foreverAsync(0)
//         else IO.unit >> foreverAsync(i + 1)

//       val task = for {
//         d <- Deferred[IO, Unit]
//         latch <- Deferred[IO, Unit]
//         fb <- (latch.complete(()) *> d.get *> foreverAsync(0)).start
//         _ <- latch.get
//         _ <- d.complete(()).timeout(15.seconds).guarantee(fb.cancel)
//       } yield {
//         Succeeded
//       }

//       task.flatMap { r =>
//         if (times > 0) execute(times - 1)
//         else IO.pure(r)
//       }
//     }

//     execute(100).unsafeToFuture()
//   }

//   test("issue #380: complete doesn't block, test #2") {
//     def execute(times: Int): IO[Assertion] = {
//       val task = for {
//         d <- Deferred[IO, Unit]
//         latch <- Deferred[IO, Unit]
//         fb <- (latch.complete(()) *> d.get *> IO.unit.foreverM).start
//         _ <- latch.get
//         _ <- d.complete(()).timeout(15.seconds).guarantee(fb.cancel)
//       } yield {
//         Succeeded
//       }

//       task.flatMap { r =>
//         if (times > 0) execute(times - 1)
//         else IO.pure(r)
//       }
//     }

//     execute(100).unsafeToFuture()
//   }
}
