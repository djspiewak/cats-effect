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

package cats.effect
package unsafe

import cats.effect.std.Semaphore
import cats.syntax.all._

import org.typelevel.scalaccompat.annotation._

import scala.annotation.tailrec
import scala.scalanative.annotation.alwaysinline
import scala.scalanative.libc.errno._
import scala.scalanative.posix.string._
import scala.scalanative.posix.unistd
import scala.scalanative.runtime._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

import java.io.IOException
import java.util.{Collections, IdentityHashMap, Set}

object EpollSystem extends PollingSystem[EpollPoller] {

  private[unsafe] final val MaxEvents = 64

  def buildRuntime(): PollingRuntime[EpollPoller] = new PollingRuntime[EpollPoller] {
    def buildPoller(reportFailure: Throwable => Unit): EpollPoller = {
      val fd = epoll.epoll_create1(0)
      if (fd == -1)
        throw new IOException(fromCString(strerror(errno)))
      new EpollPoller(fd)(reportFailure)
    }
  }

  @nowarn212
  @extern
  private[unsafe] object epoll {

    final val EPOLL_CTL_ADD = 1
    final val EPOLL_CTL_DEL = 2
    final val EPOLL_CTL_MOD = 3

    final val EPOLLIN = 0x001
    final val EPOLLOUT = 0x004
    final val EPOLLONESHOT = 1 << 30
    final val EPOLLET = 1 << 31

    type epoll_event
    type epoll_data_t = Ptr[Byte]

    def epoll_create1(flags: Int): Int = extern

    def epoll_ctl(epfd: Int, op: Int, fd: Int, event: Ptr[epoll_event]): Int = extern

    def epoll_wait(epfd: Int, events: Ptr[epoll_event], maxevents: Int, timeout: Int): Int =
      extern

  }

  private[unsafe] object epollImplicits {
    import epoll._

    implicit final class epoll_eventOps(epoll_event: Ptr[epoll_event]) {
      def events: CUnsignedInt = !epoll_event.asInstanceOf[Ptr[CUnsignedInt]]
      def events_=(events: CUnsignedInt): Unit =
        !epoll_event.asInstanceOf[Ptr[CUnsignedInt]] = events

      def data: epoll_data_t =
        !(epoll_event.asInstanceOf[Ptr[Byte]] + sizeof[CUnsignedInt])
          .asInstanceOf[Ptr[epoll_data_t]]
      def data_=(data: epoll_data_t): Unit =
        !(epoll_event.asInstanceOf[Ptr[Byte]] + sizeof[CUnsignedInt])
          .asInstanceOf[Ptr[epoll_data_t]] = data
    }

    implicit val epoll_eventTag: Tag[epoll_event] =
      Tag.materializeCArrayTag[Byte, Nat.Digit2[Nat._1, Nat._2]].asInstanceOf[Tag[epoll_event]]

  }
}
