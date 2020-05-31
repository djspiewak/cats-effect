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

import cats.{~>, Applicative, ApplicativeError, Eq, Monad, MonadError, Order, Show, Traverse}
import cats.implicits._

import scala.annotation.tailrec
import scala.util.{Either, Left, Right}

sealed trait Outcome[F[_], E, A] extends Product with Serializable {
  import Outcome._

  def fold[B](
      canceled: => B,
      errored: E => B,
      completed: F[A] => B)
      : B =
    this match {
      case Canceled() => canceled
      case Errored(e) => errored(e)
      case Completed(fa) => completed(fa)
    }

  def mapK[G[_]](f: F ~> G): Outcome[G, E, A] =
    this match {
      case Canceled() => Canceled()
      case Errored(e) => Errored(e)
      case Completed(fa) => Completed(f(fa))
    }
}

private[effect] trait LowPriorityImplicits0 {

  implicit def monowidenEffect[F[_], E, A, G[a] >: F[a]](oc: Outcome[F, E, A]): Outcome[G, E, A] =
    oc.asInstanceOf

  implicit def monowidenError[F[_], E, A, E2 >: E](oc: Outcome[F, E, A]): Outcome[F, E2, A] =
    oc.asInstanceOf

  implicit def monowidenValue[F[_], E, A, B >: A](oc: Outcome[F, E, A]): Outcome[F, E, B] =
    oc.asInstanceOf
}

private[effect] trait LowPriorityImplicits extends LowPriorityImplicits0 {
  import Outcome.{Canceled, Completed, Errored}

  // typelevel/cats#3436
  implicit def biwidenLeft[F[_], E, A, G[a] >: F[a], E2 >: E](oc: Outcome[F, E, A]): Outcome[G, E2, A] =
    oc.asInstanceOf

  // typelevel/cats#3436
  implicit def biwidenOuter[F[_], E, A, G[a] >: F[a], B >: A](oc: Outcome[F, E, A]): Outcome[G, E, B] =
    oc.asInstanceOf

  // typelevel/cats#3436
  implicit def biwidenRight[F[_], E, A, E2 >: E, B >: A](oc: Outcome[F, E, A]): Outcome[F, E2, B] =
    oc.asInstanceOf

  // variant for when F[A] doesn't have a Show (which is, like, most of the time)
  implicit def showUnknown[F[_], E, A](implicit E: Show[E]): Show[Outcome[F, E, A]] = Show show {
    case Canceled() => "Canceled"
    case Errored(left) => s"Errored(${left.show})"
    case c: Completed[_, _, _] => s"Completed(<unknown>)"
  }

  implicit def eq[F[_], E: Eq, A](implicit FA: Eq[F[A]]): Eq[Outcome[F, E, A]] = Eq instance {
    case (Canceled(), Canceled()) => true
    case (Errored(left), Errored(right)) => left === right
    case (left: Completed[F, E, A], right: Completed[F, E, A]) => left.fa === right.fa
    case _ => false
  }

  implicit def applicativeError[F[_], E](implicit F: Applicative[F]): ApplicativeError[Outcome[F, E, *], E] =
    new OutcomeApplicativeError[F, E]

  protected class OutcomeApplicativeError[F[_]: Applicative, E] extends ApplicativeError[Outcome[F, E, *], E] {

    def pure[A](x: A): Outcome[F, E, A] = Completed(x.pure[F])

    def handleErrorWith[A](fa: Outcome[F, E, A])(f: E => Outcome[F, E, A]): Outcome[F, E, A] =
      fa.fold(Canceled(), f, Completed(_: F[A]))

    def raiseError[A](e: E): Outcome[F, E, A] = Errored(e)

    def ap[A, B](ff: Outcome[F, E, A => B])(fa: Outcome[F, E, A]): Outcome[F, E, B] =
      (ff, fa) match {
        case (c: Completed[F, E, A => B], Completed(fa)) =>
          Completed(c.fa.ap(fa))

        case (Errored(e), _) =>
          Errored(e)

        case (Canceled(), _) =>
          Canceled()

        case (_, Errored(e)) =>
          Errored(e)

        case (_, Canceled()) =>
          Canceled()
      }
  }
}

object Outcome extends LowPriorityImplicits {

  def completed[F[_], A](fa: F[A]): Outcome[F, Nothing, A] =
    Completed(fa)

  def errored[E](e: E): Outcome[Nothing, E, Nothing] =
    Errored(e)

  def canceled: Outcome[Nothing, Nothing, Nothing] =
    Canceled()

  def fromEither[F[_]: Applicative, E, A](either: Either[E, A]): Outcome[F, E, A] =
    either.fold(Errored(_), a => Completed(a.pure[F]))

  // typelevel/cats#3436
  implicit def triwiden[F[_], E, A, G[a] >: F[a], E2 >: E, B >: A](oc: Outcome[F, E, A]): Outcome[G, E2, B] =
    oc.asInstanceOf

  /*implicit class Syntax[F[_], E, A](val self: Outcome[F, E, A]) extends AnyVal {

    def fold[B](
        canceled: => B,
        errored: E => B,
        completed: F[A] => B)
        : B = self match {
      case Canceled => canceled
      case Errored(e) => errored(e)
      case Completed(fa) => completed(fa)
    }

    def mapK[G[_]](f: F ~> G): Outcome[G, E, A] = self match {
      case Outcome.Canceled => Outcome.Canceled
      case Outcome.Errored(e) => Outcome.Errored(e)
      case Outcome.Completed(fa) => Outcome.Completed(f(fa))
    }
  }*/

  implicit def order[F[_], E: Order, A](implicit FA: Order[F[A]]): Order[Outcome[F, E, A]] =
    Order from {
      case (Canceled(), Canceled()) => 0
      case (Errored(left), Errored(right)) => left.compare(right)
      case (left: Completed[F, E, A], right: Completed[F, E, A]) => left.fa.compare(right.fa)

      case (Canceled(), _) => -1
      case (_, Canceled()) => 1
      case (Errored(_), _: Completed[_, _, _]) => -1
      case (_: Completed[_, _, _], Errored(_)) => 1
    }

  implicit def show[F[_], E, A](implicit FA: Show[F[A]], E: Show[E]): Show[Outcome[F, E, A]] = Show show {
    case Canceled() => "Canceled"
    case Errored(left) => s"Errored(${left.show})"
    case right: Completed[F, E, A] => s"Completed(${right.fa.show})"
  }

  implicit def monadError[F[_], E](implicit F: Monad[F], FT: Traverse[F]): MonadError[Outcome[F, E, *], E] =
    new OutcomeApplicativeError[F, E]()(F) with MonadError[Outcome[F, E, *], E] {

      override def map[A, B](fa: Outcome[F, E, A])(f: A => B): Outcome[F, E, B] = fa match {
        case c: Completed[F, E, A] => Completed(F.map(c.fa)(f))
        case Errored(e) => Errored(e)
        case Canceled() => Canceled()
      }

      def flatMap[A, B](fa: Outcome[F, E, A])(f: A => Outcome[F, E, B]): Outcome[F, E, B] = fa match {
        case ifac: Completed[F, E, A] =>
          val ifa = ifac.fa

          Traverse[F].traverse(ifa)(f) match {
            case ifaac: Completed[F, E, F[B]] => Completed(Monad[F].flatten(ifaac.fa))
            case Errored(e) => Errored(e)
            case Canceled() => Canceled()
          }

        case Errored(e) => Errored(e)
        case Canceled() => Canceled()
      }

      @tailrec
      def tailRecM[A, B](a: A)(f: A => Outcome[F, E, Either[A, B]]): Outcome[F, E, B] =
        f(a) match {
          case c: Completed[F, E, Either[A, B]] =>
            Traverse[F].sequence[Either[A, *], B](c.fa) match {   // Dotty can't infer this
              case Left(a) => tailRecM(a)(f)
              case Right(fb) => Completed(fb)
            }

          case Errored(e) => Errored(e)
          case Canceled() => Canceled()
        }
    }

  final case class Completed[F[_], E, A](fa: F[A]) extends Outcome[F, E, A]
  final case class Errored[F[_], E, A](e: E) extends Outcome[F, E, A]
  final case class Canceled[F[_], E, A]() extends Outcome[F, E, A]
}
