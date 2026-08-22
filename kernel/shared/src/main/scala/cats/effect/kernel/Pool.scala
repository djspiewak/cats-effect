package cats.effect.kernel

import scala.collection.immutable.Queue

import cats.effect.kernel.instances.spawn._
import cats.effect.kernel.syntax.all._
import cats.syntax.all._

// this is basically like a thread pool, but for fibers
private[kernel] final class Pool[F[_], E](
    work: Ref[F, Queue[F[Unit]]],
    latch: Ref[F, Option[Deferred[F, Unit]]],
    workers: Ref[F, Set[Fiber[F, E, Unit]]],
    preemption: Deferred[F, Option[E]])(implicit F: GenConcurrent[F, E]) {

  def execute(fu: F[Unit]): F[Unit] =
    work.update(_.enqueue(fu)) *> notifyReady

  def cancel: F[Unit] =
    preemption.complete(None) *> workers.get.flatMap(_.toList.parTraverse_(_.cancel))

  private def start(n: Int): F[Unit] = {
    if (n > 0) {
      F.uncancelable { _ =>
        worker.start flatMap { fiber =>
          workers.update(_ + fiber).onError { case _ => fiber.cancel }
        }
      } *> start(n - 1)
    } else {
      F.unit
    }
  }

  private def joinPreemption[A]: F[A] =
    preemption.get flatMap {
      case Some(e) => F.raiseError(e)
      case None => F.canceled *> F.never[A]
    }

  private[this] def notifyReady: F[Unit] =
    latch.getAndSet(None) flatMap {
      case Some(d) => d.complete(()).void
      case None => F.unit
    }

  private[this] def awaitReady: F[Unit] = {
    val acquireF = F.deferred[Unit] flatMap { latch0 =>
      latch modify {
        case Some(latch0) => (Some(latch0), latch0)
        case None => (Some(latch0), latch0)
      }
    }

    acquireF flatMap { ready => work.get.map(_.nonEmpty).ifM(notifyReady, F.unit) *> ready.get }
  }

  // when a worker self-cancels, we just allow it to die without replacement
  // this is sane because the outer process is also canceling in that case
  private[this] def worker: F[Unit] = {
    val next = work.modify(_.dequeueOption.map(_.swap).sequence)
    val step = next flatMap {
      case Some(fu) => fu
      case None => awaitReady
    }

    val guarded = step guaranteeCase {
      case Outcome.Succeeded(_) => F.unit
      case Outcome.Errored(e) => preemption.complete(Some(e)).void
      case Outcome.Canceled() => preemption.complete(None).void
    }

    // errors and self-cancelation torpedo the worker
    guarded >> worker
  }
}

private[kernel] object Pool {
  def apply[F[_], E, A](n: Int)(f: Pool[F, E] => F[A])(
      implicit F: GenConcurrent[F, E]): F[A] = {
    val poolF = (
      F.ref(Queue[F[Unit]]()),
      F.ref[Option[Deferred[F, Unit]]](None),
      F.ref(Set[Fiber[F, E, Unit]]()),
      F.deferred[Option[E]]).mapN(new Pool(_, _, _, _))

    poolF flatMap { pool =>
      (pool.start(n) *> F.race(f(pool), pool.joinPreemption[A]))
        .guarantee(pool.cancel)
        .map(_.merge)
    }
  }
}
