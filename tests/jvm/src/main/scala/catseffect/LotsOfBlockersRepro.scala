/*
 * Copyright 2020-2022 Typelevel
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

package catseffect

import cats.effect.{IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration._

object LotsOfBlockersRepro extends IOApp.Simple {

  override val runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  val struct = List.fill(100000)(())

  val run = {
    val update = IO.realTimeInstant.flatMap(inst => IO.println(s"[$inst] still alive"))
    (struct.parTraverse_(_ => IO.blocking(Thread.sleep(1000))) *> update).foreverM
  }
}
