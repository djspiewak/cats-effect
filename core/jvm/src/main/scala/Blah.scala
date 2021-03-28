/*
 * Copyright 2020-2021 Typelevel
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

import cats.effect.unsafe.implicits._

import scala.concurrent.ExecutionContext

import java.util.concurrent.{Executors, CountDownLatch}
import java.util.concurrent.atomic.AtomicReference

object Blah {
  private val ref = new AtomicReference[IOFiber[_]]()

  def main(args: Array[String]): Unit = {
    val pool = Executors.newFixedThreadPool(16)
    val ec = ExecutionContext.fromExecutor(pool)

    val task = (0 until 100).foldLeft(IO(10))((acc, _) =>
      IO.racePair(acc, IO(1)).flatMap {
        case Left((oc, fiber)) =>
          fiber.cancel.flatMap(_ => oc.embedNever)
        case Right((fiber, oc)) =>
          fiber.cancel.flatMap(_ => oc.embedNever)
      })/*.evalOn(ec)*/

    var i = 0
    while (true) {
      val latch = new CountDownLatch(1)
      val fiber = task.unsafeRunFiber((), _ => (), _ => latch.countDown())
      ref.set(fiber)
      latch.await()
      i += 1
      if (i % 100000 == 0) {
        println(i)
      }
    }
  }
}
