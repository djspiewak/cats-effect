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

package cats.effect.kernel

import cats.{ApplicativeError, MonadError}
import cats.data.OptionT
import cats.implicits._

// represents the type Bracket | Region
sealed trait Safe[F[_], E] extends MonadError[F, E] {

  type Case[A]

  implicit def CaseInstance: ApplicativeError[Case, E]
}

trait Bracket[F[_], E] extends Safe[F, E] {

  def handleCaseWith[A](fa: F[A])(
      a: A => F[A],
      e: E => F[A],
      c: Case[A] => F[Unit])
      : F[A]

  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] =
    handleCaseWith(fa)(a = pure(_), e = f, c = _ => unit)

  def onCase[A](fa: F[A])(pf: PartialFunction[Case[A], F[Unit]]): F[A] =
    handleCaseWith(fa)(
      a = a => pf.lift(a.pure[Case]).map(as(_, a)).getOrElse(pure(a)),

      e = { e =>
        val ran = pf.lift(CaseInstance.raiseError(e))
        val rethrow = raiseError[A](e)
        ran.map(flatMap(_)(_ => rethrow)).getOrElse(rethrow)
      },

      c = c => pf.lift(c).getOrElse(unit))
}

object Bracket {
  type Aux[F[_], E, Case0[_]] = Bracket[F, E] { type Case[A] = Case0[A] }
  type Aux2[F[_], E, Case0[_, _]] = Bracket[F, E] { type Case[A] = Case0[E, A] }

  def apply[F[_], E](implicit F: Bracket[F, E]): F.type = F
  def apply[F[_]](implicit F: Bracket[F, _], d: DummyImplicit): F.type = F

  implicit def bracketForEither[E]: Bracket.Aux2[Either[E, *], E, Either] =
    new Bracket[Either[E, *], E] {
      private[this] val delegate = catsStdInstancesForEither[E]

      type Case[A] = Either[E, A]

      def CaseInstance = this

      // export delegate.{pure, handleErrorWith, raiseError, flatMap, tailRecM}

      def pure[A](x: A): Either[E, A] =
        delegate.pure(x)

      def handleCaseWith[A](fa: Either[E, A])(
          a: A => Either[E, A],
          e: E => Either[E, A],
          c: Case[A] => Either[E, Unit])
          : Either[E, A] =
        fa match {
          case Left(err) => e(err)
          case Right(value) => a(value)
        }

      def raiseError[A](e: E): Either[E, A] =
        delegate.raiseError(e)

      def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        delegate.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] =
        delegate.tailRecM(a)(f)
    }

  implicit def bracketForOptionT[F[_], E](
      implicit F: Bracket[F, E])
      : Bracket.Aux[OptionT[F, *], E, OptionT[F.Case, *]] =
    new Bracket[OptionT[F, *], E] {

      private[this] val delegate = OptionT.catsDataMonadErrorForOptionT[F, E]

      type Case[A] = OptionT[F.Case, A]

      // TODO put this into cats-core
      def CaseInstance: ApplicativeError[Case, E] = new ApplicativeError[Case, E] {

        def pure[A](x: A): OptionT[F.Case, A] =
          OptionT.some[F.Case](x)(F.CaseInstance)

        def handleErrorWith[A](fa: OptionT[F.Case, A])(f: E => OptionT[F.Case, A]): OptionT[F.Case, A] =
          OptionT(F.CaseInstance.handleErrorWith(fa.value)(f.andThen(_.value)))

        def raiseError[A](e: E): OptionT[F.Case, A] =
          OptionT.liftF(F.CaseInstance.raiseError[A](e))(F.CaseInstance)

        def ap[A, B](ff: OptionT[F.Case, A => B])(fa: OptionT[F.Case, A]): OptionT[F.Case, B] =
          OptionT {
            F.CaseInstance.map(F.CaseInstance.product(ff.value, fa.value)) {
              case (optfab, opta) => (optfab, opta).mapN(_(_))
            }
          }
      }

      def pure[A](x: A): OptionT[F, A] =
        delegate.pure(x)

      def raiseError[A](e: E): OptionT[F, A] =
        delegate.raiseError(e)

      def handleCaseWith[A](fa: OptionT[F, A])(
          a: A => OptionT[F, A],
          e: E => OptionT[F, A],
          c: Case[A] => OptionT[F, Unit])
          : OptionT[F, A] =
        OptionT {
          F.handleCaseWith[Option[A]](fa.value)(
            a = _.flatTraverse(a.andThen(_.value)),
            e = e(_).value,

            c = { (cs: F.Case[Option[A]]) =>
              c(OptionT(cs)).value.void
            })
        }

      /*def bracketCase[A, B](
          acquire: OptionT[F, A])(
          use: A => OptionT[F, B])(
          release: (A, Case[B]) => OptionT[F, Unit])
          : OptionT[F, B] =
        OptionT {
          F.bracketCase(
            acquire.value)(
            (optA: Option[A]) => optA.flatTraverse(a => use(a).value))(
            { (optA: Option[A], resultOpt: F.Case[Option[B]]) =>
              val resultsF: F[Option[Unit]] = optA flatTraverse { a =>
                release(a, OptionT(resultOpt)).value
              }

              resultsF.void
            })
        }*/

      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
        delegate.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
        delegate.tailRecM(a)(f)
    }
}

trait Region[R[_[_], _], F[_], E] extends Safe[R[F, *], E] {

  def openCase[A, e](acquire: F[A])(release: (A, Case[e]) => F[Unit]): R[F, A]

  def open[A](acquire: F[A])(release: A => F[Unit]): R[F, A] =
    openCase(acquire)((a: A, _: Case[Unit]) => release(a))

  def liftF[A](fa: F[A]): R[F, A]

  // this is analogous to *>, but with more constrained laws (closing the resource scope)
  def supersededBy[B, e](rfa: R[F, e], rfb: R[F, B]): R[F, B]

  // this is analogous to void, but it closes the resource scope
  def close[e](rfa: R[F, e]): R[F, Unit] = supersededBy(rfa, unit)
}

object Region {
  type Aux[R[_[_], _], F[_], E, Case0[_]] = Region[R, F, E] { type Case[A] = Case0[A] }
  type Aux2[R[_[_], _], F[_], E, Case0[_, _]] = Region[R, F, E] { type Case[A] = Case0[E, A] }

  def apply[R[_[_], _], F[_], E](implicit R: Region[R, F, E]): R.type = R
  def apply[R[_[_], _], F[_]](implicit R: Region[R, F, _], d1: DummyImplicit): R.type = R
}
