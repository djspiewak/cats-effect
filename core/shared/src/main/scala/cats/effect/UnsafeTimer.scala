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

import scala.concurrent.duration.FiniteDuration

import java.util.concurrent.ScheduledExecutorService

trait UnsafeTimer {

  /**
   * Schedules a side-effect to run after the delay interval. Produces
   * another side-effect which cancels the scheduling.
   */
  def sleep(delay: FiniteDuration, task: Runnable): Runnable
}

object UnsafeTimer {
  def fromScheduledExecutor(scheduler: ScheduledExecutorService): UnsafeTimer =
    new UnsafeTimer {
      def sleep(delay: FiniteDuration, task: Runnable): Runnable = {
        val future = scheduler.schedule(task, delay.length, delay.unit)
        () => future.cancel(false)
      }
    }
}