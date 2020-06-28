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
package laws

import cats.effect.kernel.Bracket
import cats.laws.MonadErrorLaws

trait BracketLaws[F[_], E] extends MonadErrorLaws[F, E] {

  implicit val F: Bracket[F, E]

  import F.CaseInstance

  def bracketPureCoherence[A, B](acq: F[A], f: A => B, release: F.Case[B] => F[Unit]) =
    F.bracketCase(acq)(a => F.pure(f(a)))((_, c) => release(c)) <->
      F.bracketCase(acq)(a => F.pure(f(a)))((a, _) => release(CaseInstance.pure(f(a))))

  def bracketErrorCoherence[A](acq: F[A], f: A => E, release: F.Case[Unit] => F[Unit]) =
    F.bracketCase(acq)(a => F.raiseError[Unit](f(a)))((_, c) => release(c)) <->
      F.bracketCase(acq)(a => F.raiseError[Unit](f(a)))((a, _) => release(CaseInstance.raiseError(f(a))))

  def bracketAcquireErrorIdentity[A, B](e: E, f: A => F[B], release: F[Unit]) =
    F.bracketCase(F.raiseError[A](e))(f)((_, _) => release) <-> F.raiseError[B](e)

  def bracketReleaseErrorIgnore(e: E) =
    F.bracketCase(F.unit)(_ => F.unit)((_, _) => F.raiseError[Unit](e)) <-> F.unit

  def bracketBodyIdentity[A](fa: F[A]) =
    F.bracketCase(F.unit)(_ => fa)((_, _) => F.unit) <-> fa

  def onCaseDefinedByBracketCase[A](fa: F[A], pf: PartialFunction[F.Case[A], F[Unit]]) =
    F.onCase(fa)(pf) <-> F.bracketCase(F.unit)(_ => fa)((_, c) => pf.lift(c).getOrElse(F.unit))

  def onCaseConsistentFlatTap[A](a: A, f: F.Case[A] => F[Unit]) =
    F.onCase(F.pure(a))(PartialFunction.fromFunction(f)) <->
      F.flatTap(F.pure(a))(a => F.attempt(f(CaseInstance.pure(a))))

  def onCaseConsistentOnError[A](e: E, pf: PartialFunction[F.Case[A], F[Unit]]) =
    F.onCase(F.raiseError[A](e))(pf) <->
      F.onError(F.raiseError[A](e))(PartialFunction.fromFunction(CaseInstance.raiseError[A](_)).andThen(pf).andThen(fu => F.void(F.attempt(fu))))
}

object BracketLaws {
  def apply[F[_], E](implicit F0: Bracket[F, E]): BracketLaws[F, E] { val F: F0.type } =
    new BracketLaws[F, E] { val F: F0.type = F0 }
}
