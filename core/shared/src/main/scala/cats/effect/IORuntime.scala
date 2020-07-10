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

import scala.concurrent.ExecutionContext
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors

case class IORuntime(compute: ExecutionContext, timer: UnsafeTimer) extends IORuntimePlatform {

  def unsafeRunAsync[A](ioa: IO[A])(
      cb: Either[Throwable, A] => Unit)
      : Unit =
    unsafeRunFiber(ioa, true)(cb)

  private[effect] def unsafeRunFiber[A](
      ioa: IO[A],
      shift: Boolean)(
      cb: Either[Throwable, A] => Unit)
      : IOFiber[A] = {

    val fiber = new IOFiber(
      timer,
      (oc: Outcome[IO, Throwable, A]) => oc.fold(
        (),
        e => cb(Left(e)),
        ioa => cb(Right(ioa.asInstanceOf[IO.Pure[A]].value))),
      0)

    if (shift)
      compute.execute(() => fiber.run(ioa, compute, 0))
    else
      fiber.run(ioa, compute, 0)

    fiber
  }
}

object IORuntime extends IORuntimeCompanionPlatform