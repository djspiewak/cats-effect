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

final class KqueuePoller(kqfd: Int)(reportFailure: Throwable => Unit) extends FileDescriptorPoller {
  import KqueueSystem.{KEvent, MaxEvents}
  import KqueueSystem.event._
  import KqueueSystem.eventImplicits._

  private[this] val changelistArray = new Array[Byte](sizeof[kevent64_s].toInt * MaxEvents)
  private[this] val changelist = changelistArray.at(0).asInstanceOf[Ptr[kevent64_s]]
  private[this] var changeCount = 0

  private[this] val callbacks = new HashMap[KEvent, Either[Throwable, Unit] => Unit]()

  private[unsafe] def evSet(
      event: KEvent,
      flags: CUnsignedShort,
      cb: Either[Throwable, Unit] => Unit)
      : Unit = {
    val change = changelist + changeCount.toLong

    change.ident = event.ident.toULong
    change.filter = event.filter
    change.flags = (flags.toInt | EV_ONESHOT).toUShort

    callbacks.put(event, cb)

    changeCount += 1
  }

  private[unsafe] def removeCallback(event: KEvent): Unit = {
    callbacks.remove(event)
    ()
  }

  def close(): Unit =
    if (unistd.close(kqfd) != 0)
      throw new IOException(fromCString(strerror(errno)))

  def interrupt(targetThread: Thread): Unit = ()    // never called

  def poll(timeout: Long): Boolean = {
    val noCallbacks = callbacks.isEmpty

    if (timeout <= 0 && noCallbacks && changeCount == 0)
      false // nothing to do here
    else {

      val eventlist = stackalloc[kevent64_s](MaxEvents.toLong)

      @tailrec
      def processEvents(timeout: Ptr[timespec], changeCount: Int, flags: Int): Unit = {

        val triggeredEvents =
          kevent64(
            kqfd,
            changelist,
            changeCount,
            eventlist,
            MaxEvents,
            flags.toUInt,
            timeout
          )

        if (triggeredEvents >= 0) {
          var i = 0
          var event = eventlist
          while (i < triggeredEvents) {
            val cb = callbacks.remove(KEvent(event.ident.toLong, event.filter))

            if (cb ne null)
              cb(
                if ((event.flags.toLong & EV_ERROR) != 0)
                  Left(new IOException(fromCString(strerror(event.data.toInt))))
                else Either.unit
              )

            i += 1
            event += 1
          }
        } else {
          throw new IOException(fromCString(strerror(errno)))
        }

        if (triggeredEvents >= MaxEvents)
          processEvents(null, 0, KEVENT_FLAG_NONE) // drain the ready list
        else
          ()
      }

      val timeoutSpec =
        if (timeout <= 0) null
        else {
          val ts = stackalloc[timespec]()
          ts.tv_sec = timeout / 1000000000
          ts.tv_nsec = timeout % 1000000000
          ts
        }

      val flags = if (timeout == 0) KEVENT_FLAG_IMMEDIATE else KEVENT_FLAG_NONE

      processEvents(timeoutSpec, changeCount, flags)
      changeCount = 0

      !callbacks.isEmpty()
    }
  }

  def registerFileDescriptor(
      fd: Int,
      reads: Boolean,
      writes: Boolean,
      readS: Semaphore[IO],
      writeS: Semaphore[IO])
      : (FileDescriptorPollHandle, () => Unit) =
    new PollHandle(fd, readS, writeS)

  private final class PollHandle(
      register: (Poller => Unit) => Unit,
      fd: Int,
      readSemaphore: Semaphore[IO],
      writeSemaphore: Semaphore[IO]
  ) extends FileDescriptorPollHandle {

    private[this] val readEvent = KEvent(fd.toLong, EVFILT_READ)
    private[this] val writeEvent = KEvent(fd.toLong, EVFILT_WRITE)

    def pollReadRec[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      readSemaphore.permit.surround {
        a.tailRecM { a =>
          f(a).flatTap { r =>
            if (r.isRight)
              IO.unit
            else
              IO.async[Unit] { kqcb =>
                IO.async_[Option[IO[Unit]]] { cb =>
                  register { kqueue =>
                    kqueue.evSet(readEvent, EV_ADD.toUShort, kqcb)
                    cb(Right(Some(IO(kqueue.removeCallback(readEvent)))))
                  }
                }

              }
          }
        }
      }

    def pollWriteRec[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      writeSemaphore.permit.surround {
        a.tailRecM { a =>
          f(a).flatTap { r =>
            if (r.isRight)
              IO.unit
            else
              IO.async[Unit] { kqcb =>
                IO.async_[Option[IO[Unit]]] { cb =>
                  register { kqueue =>
                    kqueue.evSet(writeEvent, EV_ADD.toUShort, kqcb)
                    cb(Right(Some(IO(kqueue.removeCallback(writeEvent)))))
                  }
                }
              }
          }
        }
      }

  }
}
