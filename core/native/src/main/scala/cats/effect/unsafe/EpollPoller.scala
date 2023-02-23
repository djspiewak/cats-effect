package cats.effect
package unsafe

import cats.effect.std.Semaphore

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

final class EpollPoller(epfd: Int)(reportFailure: Throwable => Unit) extends FileDescriptorPoller {
  import EpollSystem._

  import epoll._
  import epollImplicits._

  private[this] val handles: Set[PollHandle] =
    Collections.newSetFromMap(new IdentityHashMap)

  def close(): Unit =
    if (unistd.close(epfd) != 0)
      throw new IOException(fromCString(strerror(errno)))

  def interrupt(targetThread: Thread): Unit = ()    // never called

  def poll(timeout: Long): Boolean = {
    val noHandles = handles.isEmpty()

    if (timeout <= 0 && noHandles)
      false // nothing to do here
    else {
      val events = stackalloc[epoll_event](MaxEvents.toLong)

      @tailrec
      def processEvents(timeout: Int): Unit = {

        val triggeredEvents = epoll_wait(epfd, events, MaxEvents, timeout)

        if (triggeredEvents >= 0) {
          var i = 0
          while (i < triggeredEvents) {
            val event = events + i.toLong
            val handle = fromPtr(event.data)
            handle.notify(event.events.toInt)
            i += 1
          }
        } else {
          throw new IOException(fromCString(strerror(errno)))
        }

        if (triggeredEvents >= MaxEvents)
          processEvents(0) // drain the ready list
        else
          ()
      }

      val timeoutMillis = if (timeout == -1) -1 else (timeout / 1000000).toInt
      processEvents(timeoutMillis)

      !handles.isEmpty()
    }
  }

  def registerFileDescriptor(
      fd: Int,
      reads: Boolean,
      writes: Boolean,
      readS: Semaphore[IO],
      writeS: Semaphore[IO])
      : (FileDescriptorPollHandle, () => Unit) = {
    val handle = new PollHandle(readS, writeS)
    val unregister = register(fd, reads, writes, handle)
    (handle, unregister)
  }

  private[this] def register(
      fd: Int,
      reads: Boolean,
      writes: Boolean,
      handle: PollHandle)
      : () => Unit = {

    val event = stackalloc[epoll_event]()
    event.events =
      (EPOLLET | (if (reads) EPOLLIN else 0) | (if (writes) EPOLLOUT else 0)).toUInt
    event.data = toPtr(handle)

    if (epoll_ctl(epfd, EPOLL_CTL_ADD, fd, event) != 0)
      throw new IOException(fromCString(strerror(errno)))
    handles.add(handle)

    { () =>
      handles.remove(handle)
      if (epoll_ctl(epfd, EPOLL_CTL_DEL, fd, null) != 0)
        throw new IOException(fromCString(strerror(errno)))
    }
  }

  @alwaysinline private[this] def toPtr(handle: PollHandle): Ptr[Byte] =
    fromRawPtr(Intrinsics.castObjectToRawPtr(handle))

  @alwaysinline private[this] def fromPtr[A](ptr: Ptr[Byte]): PollHandle =
    Intrinsics.castRawPtrToObject(toRawPtr(ptr)).asInstanceOf[PollHandle]

  private final class PollHandle(
      readSemaphore: Semaphore[IO],
      writeSemaphore: Semaphore[IO]
  ) extends FileDescriptorPollHandle {

    private[this] var readReadyCounter = 0
    private[this] var readCallback: Either[Throwable, Int] => Unit = null

    private[this] var writeReadyCounter = 0
    private[this] var writeCallback: Either[Throwable, Int] => Unit = null

    def notify(events: Int): Unit = {
      if ((events & EPOLLIN) != 0) {
        val counter = readReadyCounter + 1
        readReadyCounter = counter
        val cb = readCallback
        readCallback = null
        if (cb ne null) cb(Right(counter))
      }
      if ((events & EPOLLOUT) != 0) {
        val counter = writeReadyCounter + 1
        writeReadyCounter = counter
        val cb = writeCallback
        writeCallback = null
        if (cb ne null) cb(Right(counter))
      }
    }

    def pollReadRec[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      readSemaphore.permit.surround {
        def go(a: A, before: Int): IO[B] =
          f(a).flatMap {
            case Left(a) =>
              IO(readReadyCounter).flatMap { after =>
                if (before != after)
                  // there was a read-ready notification since we started, try again immediately
                  go(a, after)
                else
                  IO.asyncCheckAttempt[Int] { cb =>
                    IO {
                      readCallback = cb
                      // check again before we suspend
                      val now = readReadyCounter
                      if (now != before) {
                        readCallback = null
                        Right(now)
                      } else Left(Some(IO(this.readCallback = null)))
                    }
                  }.flatMap(go(a, _))
              }
            case Right(b) => IO.pure(b)
          }

        IO(readReadyCounter).flatMap(go(a, _))
      }

    def pollWriteRec[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      writeSemaphore.permit.surround {
        def go(a: A, before: Int): IO[B] =
          f(a).flatMap {
            case Left(a) =>
              IO(writeReadyCounter).flatMap { after =>
                if (before != after)
                  // there was a write-ready notification since we started, try again immediately
                  go(a, after)
                else
                  IO.asyncCheckAttempt[Int] { cb =>
                    IO {
                      writeCallback = cb
                      // check again before we suspend
                      val now = writeReadyCounter
                      if (now != before) {
                        writeCallback = null
                        Right(now)
                      } else Left(Some(IO(this.writeCallback = null)))
                    }
                  }.flatMap(go(a, _))
              }
            case Right(b) => IO.pure(b)
          }

        IO(writeReadyCounter).flatMap(go(a, _))
      }
  }
}
