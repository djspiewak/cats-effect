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

  def handleCaseWithCoherence[A](fa: F[A], result: A => F[A], err: E => F[A]) =
    F.handleCaseWith(fa)(a = result, e = err, c = _ => F.unit) <->
      F.handleErrorWith(F.flatMap(fa)(result))(err)

  def onCasePureCoherence[A](a: A, release: PartialFunction[F.Case[A], F[Unit]]) =
    F.onCase(F.pure(a))(release) <-> F.as(release.lift(CaseInstance.pure(a)).getOrElse(F.unit), a)

  def onCaseErrorCoherence(e: E, release: PartialFunction[F.Case[Unit], F[Unit]]) =
    F.onCase(F.raiseError[Unit](e))(release) <-> release.lift(CaseInstance.raiseError[Unit](e)).getOrElse(F.unit)
}

object BracketLaws {
  def apply[F[_], E](implicit F0: Bracket[F, E]): BracketLaws[F, E] { val F: F0.type } =
    new BracketLaws[F, E] { val F: F0.type = F0 }
}
