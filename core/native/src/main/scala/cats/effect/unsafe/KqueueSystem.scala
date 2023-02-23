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
import scala.scalanative.libc.errno._
import scala.scalanative.posix.string._
import scala.scalanative.posix.time._
import scala.scalanative.posix.timeOps._
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

import java.io.IOException
import java.util.HashMap

object KqueueSystem extends PollingSystem {

  import event._
  import eventImplicits._

  private[unsafe] final val MaxEvents = 64

  def buildRuntime(): PollingRuntime[KqueuePoller] =
    new PollingRuntime[KqueuePoller] {
      def buildPoller(reportFailure: Throwable => Unit): KqueuePoller = {
        val fd = kqueue()
        if (fd == -1)
          throw new IOException(fromCString(strerror(errno)))
        new KqueuePoller(fd)(reportFailure)
      }
    }

  private[unsafe] final case class KEvent(ident: Long, filter: Short)

  @nowarn212
  @extern
  private[unsafe] object event {
    // Derived from https://opensource.apple.com/source/xnu/xnu-7195.81.3/bsd/sys/event.h.auto.html

    final val EVFILT_READ = -1
    final val EVFILT_WRITE = -2

    final val KEVENT_FLAG_NONE = 0x000000
    final val KEVENT_FLAG_IMMEDIATE = 0x000001

    final val EV_ADD = 0x0001
    final val EV_DELETE = 0x0002
    final val EV_ONESHOT = 0x0010
    final val EV_CLEAR = 0x0020
    final val EV_ERROR = 0x4000

    type kevent64_s

    def kqueue(): CInt = extern

    def kevent64(
        kq: CInt,
        changelist: Ptr[kevent64_s],
        nchanges: CInt,
        eventlist: Ptr[kevent64_s],
        nevents: CInt,
        flags: CUnsignedInt,
        timeout: Ptr[timespec]
    ): CInt = extern

  }

  private[unsafe] object eventImplicits {

    implicit final class kevent64_sOps(kevent64_s: Ptr[kevent64_s]) {
      def ident: CUnsignedLongInt = !kevent64_s.asInstanceOf[Ptr[CUnsignedLongInt]]
      def ident_=(ident: CUnsignedLongInt): Unit =
        !kevent64_s.asInstanceOf[Ptr[CUnsignedLongInt]] = ident

      def filter: CShort = !(kevent64_s.asInstanceOf[Ptr[CShort]] + 4)
      def filter_=(filter: CShort): Unit =
        !(kevent64_s.asInstanceOf[Ptr[CShort]] + 4) = filter

      def flags: CUnsignedShort = !(kevent64_s.asInstanceOf[Ptr[CUnsignedShort]] + 5)
      def flags_=(flags: CUnsignedShort): Unit =
        !(kevent64_s.asInstanceOf[Ptr[CUnsignedShort]] + 5) = flags

      def data: CLong = !(kevent64_s.asInstanceOf[Ptr[CLong]] + 2)

      def udata: Ptr[Byte] = !(kevent64_s.asInstanceOf[Ptr[Ptr[Byte]]] + 3)
      def udata_=(udata: Ptr[Byte]): Unit =
        !(kevent64_s.asInstanceOf[Ptr[Ptr[Byte]]] + 3) = udata
    }

    implicit val kevent64_sTag: Tag[kevent64_s] =
      Tag.materializeCArrayTag[Byte, Nat.Digit2[Nat._4, Nat._8]].asInstanceOf[Tag[kevent64_s]]
  }
}
