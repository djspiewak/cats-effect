/*
 * Copyright 2020 Daniel Spiewak
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

package ce3

import cats.{~>, Eq, Functor, Id, Monad, MonadError, Show}
import cats.data.{Kleisli, WriterT}
import cats.free.FreeT
import cats.implicits._

import cats.mtl.ApplicativeAsk
import cats.mtl.implicits._

import coop.{ApplicativeThread, ThreadT, MVar}

object playground {

  type IdOC[E, A] = Outcome[Id, E, A]                       // a fiber may complete, error, or cancel
  type FiberR[E, A] = Kleisli[IdOC[E, ?], FiberCtx[E], A]   // fiber context and results
  type MVarR[F[_], A] = Kleisli[F, MVar.Universe, A]        // ability to use MVar(s)

  type PureConc[E, A] = MVarR[ThreadT[FiberR[E, ?], ?], A]

  type Finalizer[E] = Outcome[PureConc[E, ?], E, Nothing] => PureConc[E, Unit]

  final class MaskId

  object MaskId {
    implicit val eq: Eq[MaskId] = Eq.fromUniversalEquals[MaskId]
  }

  final case class FiberCtx[E](self: PureFiber[E, _], masks: List[MaskId] = Nil)

  type ResolvedPC[E, A] = ThreadT[IdOC[E, ?], A]

  def resolveMain[E, A](pc: PureConc[E, A]): ResolvedPC[E, IdOC[E, A]] = {
    /*
     * The cancelation implementation is here. The failures of type inference make this look
     * HORRIBLE but the general idea is fairly simple: mapK over the FreeT into a new monad
     * which sequences a cancelation check within each flatten. Thus, we go from Kleisli[FreeT[Kleisli[Outcome[Id, ...]]]]
     * to Kleisli[FreeT[Kleisli[FreeT[Kleisli[Outcome[Id, ...]]]]]]]], which we then need to go
     * through and flatten. The cancelation check *itself* is in `cancelationCheck`, while the flattening
     * process is in the definition of `val canceled`.
     *
     * FlatMapK and TraverseK typeclasses would make this a one-liner.
     */

    val cancelationCheck = new (FiberR[E, ?] ~> PureConc[E, ?]) {
      def apply[α](ka: FiberR[E, α]): PureConc[E, α] = {
        val back = Kleisli.ask[IdOC[E, ?], FiberCtx[E]] map { ctx =>
          // we have to double check finalization so that we don't accidentally cancel the finalizer
          // this is also where we check masking to ensure that we don't abort out in the middle of a masked section
          (ctx.self.canceled, ctx.self.finalizing).mapN(_ && !_ && ctx.masks.isEmpty).ifM(
            ApplicativeThread[PureConc[E, ?]].done,
            mvarLiftF(ThreadT.liftF(ka)))
        }

        mvarLiftF(ThreadT.liftF(back)).flatten
      }
    }

    // flatMapF does something different
    val canceled = Kleisli { (u: MVar.Universe) =>
      val outerStripped = pc.mapF(_.mapK(cancelationCheck)).run(u)    // run the outer mvar kleisli
      val traversed = outerStripped.mapK(λ[PureConc[E, ?] ~> ThreadT[FiberR[E, ?], ?]](_.run(u)))    // run the inner mvar kleisli
      flattenK(traversed)
    }

    val backM = MVar resolve {
      // this is PureConc[E, ?] without the inner Kleisli
      type Main[X] = MVarR[ResolvedPC[E, ?], X]

      MVar.empty[Main, Outcome[PureConc[E, ?], E, A]] flatMap { state0 =>
        val state = state0[Main]

        MVar[Main, List[Finalizer[E]]](Nil) flatMap { finalizers =>
          val fiber = new PureFiber[E, A](state0, finalizers)

          val identified = pc /*canceled*/ mapF { ta =>
            ta mapK λ[FiberR[E, ?] ~> IdOC[E, ?]] { ke =>
              ke.run(FiberCtx(fiber))
            }
          }

          import Outcome._

          val body = identified.flatMap(a => state.tryPut(Completed(a.pure[PureConc[E, ?]]))) handleErrorWith { e =>
            state.tryPut(Errored(e))
          }

          val results = state.read flatMap {
            case Canceled => (Canceled: IdOC[E, A]).pure[Main]
            case Errored(e) =>
              (Errored(e): IdOC[E, A]).pure[Main]

            case Completed(fa) =>
              val identified = fa mapF { ta =>
                ta mapK λ[FiberR[E, ?] ~> IdOC[E, ?]] { ke =>
                  ke.run(FiberCtx(fiber))
                }
              }

              identified.map(a => Completed[Id, A](a): IdOC[E, A]) handleError { e =>
                Errored(e)
              }
          }

          Kleisli.ask[ResolvedPC[E, ?], MVar.Universe] map { u =>
            body.run(u) >> results.run(u)
          }
        }
      }
    }

    backM.flatten
  }

  /**
   * Produces Completed(None) when the main fiber is deadlocked. Note that
   * deadlocks outside of the main fiber are ignored when results are
   * appropriately produced (i.e. daemon semantics).
   */
  def run[E, A](pc: PureConc[E, A]): Outcome[Option, E, A] = {
    val scheduled = ThreadT roundRobin {
      // we put things into WriterT because roundRobin returns Unit
      resolveMain(pc).mapK(WriterT.liftK[IdOC[E, ?], List[IdOC[E, A]]]) flatMap { ec =>
        ThreadT liftF {
          WriterT.tell[IdOC[E, ?], List[IdOC[E, A]]](List(ec))
        }
      }
    }

    scheduled.run.mapK(λ[Id ~> Option](Some(_))) flatMap {
      case (List(results), _) => results.mapK(λ[Id ~> Option](Some(_)))
      case (_, false) => Outcome.Completed(None)

      // we could make a writer that only receives one object, but that seems meh. just pretend we deadlocked
      case o =>
        println(s"uh? $o")
        Outcome.Completed(None)
    }
  }

  // the one in Free is broken: typelevel/cats#3240
  implicit def catsFreeMonadErrorForFreeT2[
      S[_],
      M[_],
      E](
      implicit E: MonadError[M, E],
      S: Functor[S])
      : MonadError[FreeT[S, M, *], E] =
    new MonadError[FreeT[S, M, *], E] {
      private val F = FreeT.catsFreeMonadErrorForFreeT[S, M, E]

      def pure[A](x: A): FreeT[S, M, A] =
        F.pure(x)

      def flatMap[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] =
        F.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => FreeT[S, M, Either[A, B]]): FreeT[S, M, B] =
        F.tailRecM(a)(f)

      // this is the thing we need to override
      def handleErrorWith[A](fa: FreeT[S, M, A])(f: E => FreeT[S, M, A]) = {
        val ft = FreeT liftT[S, M, FreeT[S, M, A]] {
          val resultsM = fa.resume map {
            case Left(se) =>
              pure(()).flatMap(_ => FreeT.roll(se.map(handleErrorWith(_)(f))))

            case Right(a) =>
              pure(a)
          }

          resultsM handleErrorWith { e =>
            f(e).resume map { eth =>
              FreeT.defer(eth.swap.pure[M]) // why on earth is defer inconsistent with resume??
            }
          }
        }

        ft.flatMap(identity)
      }

      def raiseError[A](e: E) =
        F.raiseError(e)
    }

  implicit def concurrentBForPureConc[E]: ConcurrentBracket[PureConc[E, ?], E] =
    new Concurrent[PureConc[E, ?], E] with Bracket[PureConc[E, ?], E] {
      private[this] val M: MonadError[PureConc[E, ?], E] =
        Kleisli.catsDataMonadErrorForKleisli

      private[this] val Thread = ApplicativeThread[PureConc[E, ?]]

      def pure[A](x: A): PureConc[E, A] =
        M.pure(x)

      def handleErrorWith[A](fa: PureConc[E, A])(f: E => PureConc[E, A]): PureConc[E, A] =
        M.handleErrorWith(fa)(f)

      def raiseError[A](e: E): PureConc[E, A] =
        M.raiseError(e)

      def bracketCase[A, B](acquire: PureConc[E, A])(use: A => PureConc[E, B])(release: (A, Outcome[PureConc[E, ?], E, B]) => PureConc[E, Unit]): PureConc[E, B] =
        uncancelable { poll =>
          acquire flatMap { a =>
            val finalized = onCancel(poll(use(a)), release(a, Outcome.Canceled))
            val handled = finalized.handleErrorWith(e => release(a, Outcome.Errored(e)).attempt >> raiseError(e))
            handled.flatMap(b => release(a, Outcome.Completed(pure(b))).attempt.as(b))
          }
        }

      def onCancel[A](fa: PureConc[E, A], body: PureConc[E, Unit]): PureConc[E, A] =
        onCase(fa, body)(Outcome.Canceled ==)

      override def onCase[A](fa: PureConc[E, A], body: PureConc[E, Unit])(p: Outcome[PureConc[E, ?], E, A] => Boolean): PureConc[E, A] = {
        def pbody(oc: Outcome[PureConc[E, ?], E, A]) =    // ...and Sherman
          if (p(oc)) body.attempt.void else unit

        val finalizer: Finalizer[E] =
          ec => uncancelable(_ => pbody(ec) >> withCtx(_.self.popFinalizer))

        uncancelable { poll =>
          val handled = poll(fa).handleErrorWith(e => finalizer(Outcome.Errored(e)) >> raiseError[A](e))

          val completed = handled flatMap { a =>
            uncancelable { _ =>
              pbody(Outcome.Completed(pure(a))).as(a) <* withCtx(_.self.popFinalizer)
            }
          }

          withCtx[E, Unit](_.self.pushFinalizer(finalizer)) >> completed
        }
      }

      def canceled[A](fallback: A): PureConc[E, A] =
        withCtx(_.self.cancelB.ifM(Thread.done[A], pure(fallback)))

      def cede: PureConc[E, Unit] =
        Thread.cede

      def never[A]: PureConc[E, A] =
        Thread.done[A]

      /**
       * Whereas `start` ignores the cancelability of the parent fiber
       * when forking off the child, `racePair` inherits cancelability.
       * Thus, `uncancelable(_ => race(fa, fb)) <-> race(uncancelable(_ => fa), uncancelable(_ => fb))`,
       * while `uncancelable(_ => start(fa)) <-> start(fa)`.
       *
       * race(cede >> raiseError(e1), raiseError(e2)) <-> raiseError(e1)
       * race(raiseError(e1), cede >> raiseError(e2)) <-> raiseError(e2)
       * race(canceled(()), raiseError(e)) <-> raiseError(e)
       * race(raiseError(e), canceled(())) <-> raiseError(e)
       */
      def racePair[A, B](
          fa: PureConc[E, A],
          fb: PureConc[E, B])
          : PureConc[
            E,
            Either[
              (A, Fiber[PureConc[E, ?], E, B]),
              (Fiber[PureConc[E, ?], E, A], B)]] =
        withCtx { (ctx: FiberCtx[E]) =>
          println("----------------------")
          type Result = Either[(A, Fiber[PureConc[E, ?], E, B]), (Fiber[PureConc[E, ?], E, A], B)]

          for {
            results0 <- MVar.empty[PureConc[E, ?], Outcome[Id, E, Result]]
            _ = println(s"results0 = $results0")
            results = results0[PureConc[E, ?]]

            fiberAVar0 <- MVar.empty[PureConc[E, ?], Fiber[PureConc[E, ?], E, A]]
            fiberBVar0 <- MVar.empty[PureConc[E, ?], Fiber[PureConc[E, ?], E, B]]

            _ = println(s"fiberAVar0 = $fiberAVar0")
            _ = println(s"fiberBVar0 = $fiberBVar0")

            fiberAVar = fiberAVar0[PureConc[E, ?]]
            fiberBVar = fiberBVar0[PureConc[E, ?]]

            cancelVar0 <- MVar.empty[PureConc[E, ?], Unit]
            errorVar0 <- MVar.empty[PureConc[E, ?], E]

            _ = println(s"cancelVar0 = $cancelVar0")
            _ = println(s"errorVar0 = $errorVar0")

            cancelVar = cancelVar0[PureConc[E, ?]]
            errorVar = errorVar0[PureConc[E, ?]]

            cancelReg = cancelVar.tryRead flatMap {
              case Some(_) =>
                println(">>> another one has canceled")
                results.tryPut(Outcome.Canceled).void   // the other one has canceled, so cancel the whole

              case None =>
                println(">>> first to cancel")
                cancelVar.tryPut(()).ifM(   // we're the first to cancel
                  errorVar.tryRead flatMap {
                    case Some(e) => println(">>> someone else errored"); results.tryPut(Outcome.Errored(e)).void   // ...because the other one errored, so use that error
                    case None => println(">>>> no one else errored"); unit                                 // ...because the other one is still in progress
                  },
                  results.tryPut(Outcome.Canceled).void)      // race condition happened and both are now canceled
            }

            errorReg = { (e: E) =>
              val completeWithError = results.tryPut(Outcome.Errored(e)).map(b => {println(s"completed with error: $b"); ()})    // last wins

              println("registering error")

              errorVar.tryRead flatMap {
                case Some(_) =>
                  println("there was an old error")
                  completeWithError   // both have errored, use the last one (ours)

                case None =>
                  println("no old one")
                  errorVar.tryPut(e).ifM(   // we were the first to error
                    cancelVar.tryRead flatMap {
                      case Some(_) => println("someone else canceled"); completeWithError   // ...because the other one canceled, so use ours
                      case None => println("no cancel"); unit                   // ...because the other one is still in progress
                    },
                    completeWithError)      // both have errored, there was a race condition, and we were the loser (use our error)
              }
            }

            // we play careful tricks here to forward the masks on from the parent to the child
            // this is necessary because start drops masks
            fa2 = withCtx { (ctx2: FiberCtx[E]) =>
              val body = bracketCase(unit)(_ => fa) {
                case (_, Outcome.Completed(fa)) =>
                  println("completed a " + fb)
                  for {
                    a <- fa
                    fiberB <- fiberBVar.read
                    _ <- results.tryPut(Outcome.Completed[Id, Result](Left((a, fiberB))))
                  } yield ()

                case (_, Outcome.Errored(e)) =>
                  errorReg(e)

                case (_, Outcome.Canceled) =>
                  cancelReg
              }

              localCtx(ctx2.copy(masks = ctx2.masks ::: ctx.masks), body)
            }

            fb2 = withCtx { (ctx2: FiberCtx[E]) =>
              val body = bracketCase(unit)(_ => fb) {
                case (_, Outcome.Completed(fb)) =>
                  for {
                    b <- fb
                    fiberA <- fiberAVar.read
                    _ <- results.tryPut(Outcome.Completed[Id, Result](Right((fiberA, b))))
                  } yield ()

                case (_, Outcome.Errored(e)) =>
                  errorReg(e)

                case (_, Outcome.Canceled) =>
                  cancelReg
              }

              localCtx(ctx2.copy(masks = ctx2.masks ::: ctx.masks), body)
            }

            back <- uncancelable { poll =>
              for {
                // note that we're uncancelable here, but we captured the masks *earlier* so we forward those along, ignoring this one
                fiberA <- start(fa2)
                fiberB <- start(fb2)

                _ <- fiberAVar.put(fiberA)
                _ <- fiberBVar.put(fiberB)

                _ = println(s"awaiting results ($results0)")
                backOC <- /*onCancel(poll(*/results.read/*), fiberA.cancel >> fiberB.cancel)*/
                _ = println(s"got backOC = $backOC")

                back <- backOC match {
                  case Outcome.Completed(res) =>
                    pure(res)

                  case Outcome.Errored(e) =>
                    raiseError[Result](e)

                  /*
                   * This is REALLY tricky, but poll isn't enough here. For example:
                   *
                   * uncancelable(p => racePair(p(canceled(())), p(canceled(())))) <-> canceled(())
                   *
                   * This semantic is pretty natural, but we can't do it here without
                   * directly manipulating the masks because we don't have the outer poll!
                   * To solve this problem, we just nuke the masks and forcibly self-cancel.
                   * We don't really have to worry about nesting problems here because, if
                   * our children were somehow able to cancel, then whatever combination of
                   * masks exists must have all been polled away *there*, so we can pretend
                   * that they were similarly polled here.
                   */
                  case Outcome.Canceled =>
                    // this will only be hit if the implementation of canceled is broken and it somehow uses the fallback even when masks == Nil
                    def err: PureConc[E, Result] = sys.error("impossible")
                    localCtx(ctx.copy(masks = Nil), canceled(()) >> err)
                }
              } yield back
            }
          } yield back
        }

      def start[A](fa: PureConc[E, A]): PureConc[E, Fiber[PureConc[E, ?], E, A]] =
        MVar.empty[PureConc[E, ?], Outcome[PureConc[E, ?], E, A]] flatMap { state =>
          MVar[PureConc[E, ?], List[Finalizer[E]]](Nil) flatMap { finalizers =>
            val fiber = new PureFiber[E, A](state, finalizers)
            val identified = localCtx(FiberCtx(fiber), fa)    // note we drop masks here

            // the tryPut(s) here are interesting: they encode first-wins semantics on cancelation/completion
            val body = identified.flatMap(a => state.tryPut[PureConc[E, ?]](Outcome.Completed(a.pure[PureConc[E, ?]]))) handleErrorWith { e =>
              state.tryPut[PureConc[E, ?]](Outcome.Errored(e))
            }

            Thread.start(body).as(fiber)
          }
        }

      def uncancelable[A](body: PureConc[E, ?] ~> PureConc[E, ?] => PureConc[E, A]): PureConc[E, A] = {
        val mask = new MaskId

        val poll = λ[PureConc[E, ?] ~> PureConc[E, ?]] { fa =>
          withCtx { ctx =>
            val ctx2 = ctx.copy(masks = ctx.masks.dropWhile(mask ===))
            localCtx(ctx2, fa)
          }
        }

        withCtx { ctx =>
          val ctx2 = ctx.copy(masks = mask :: ctx.masks)
          localCtx(ctx2, body(poll))
        }
      }

      def flatMap[A, B](fa: PureConc[E, A])(f: A => PureConc[E, B]): PureConc[E, B] =
        M.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => PureConc[E, Either[A, B]]): PureConc[E, B] =
        M.tailRecM(a)(f)
    }

  implicit def pureConcEq[E: Eq, A: Eq]: Eq[PureConc[E, A]] = Eq.by(run(_))

  implicit def showPureConc[E: Show, A: Show]: Show[PureConc[E, A]] =
    Show show { pc =>
      val trace = ThreadT.prettyPrint(resolveMain(pc), limit = 1024).fold(
        "Canceled",
        e => s"Errored(${e.show})",
        str => str.replace('╭', '├'))

      run(pc).show + "\n│\n" + trace
    }

  private[this] def mvarLiftF[F[_], A](fa: F[A]): MVarR[F, A] =
    Kleisli.liftF[F, MVar.Universe, A](fa)

  // this would actually be a very usful function for FreeT to have
  private[this] def flattenK[S[_]: Functor, M[_]: Monad, A](ft: FreeT[S, FreeT[S, M, ?], A]): FreeT[S, M, A] =
    ft.resume.flatMap(_.fold(sft => FreeT.liftF[S, M, FreeT[S, FreeT[S, M, ?], A]](sft).flatMap(flattenK(_)), FreeT.pure(_)))

  // the type inferencer just... fails... completely here
  private[this] def withCtx[E, A](body: FiberCtx[E] => PureConc[E, A]): PureConc[E, A] =
    mvarLiftF(ThreadT.liftF(Kleisli.ask[IdOC[E, ?], FiberCtx[E]].map(body))).flatten
    // ApplicativeAsk[PureConc[E, ?], FiberCtx[E]].ask.flatMap(body)

  private[this] def localCtx[E, A](ctx: FiberCtx[E], around: PureConc[E, A]): PureConc[E, A] =
    around mapF { ft =>
      ft mapK λ[FiberR[E, ?] ~> FiberR[E, ?]] { ka =>
        Kleisli(_ => ka.run(ctx))
      }
    }

  final class PureFiber[E, A](
      state0: MVar[Outcome[PureConc[E, ?], E, A]],
      finalizers0: MVar[List[Finalizer[E]]])
      extends Fiber[PureConc[E, ?], E, A] {

    println(s"state0 = $state0")
    println(s"finalizers0 = $finalizers0")

    private[this] val state = state0[PureConc[E, ?]]
    private[this] val finalizers = finalizers0[PureConc[E, ?]]

    private[playground] val canceled: PureConc[E, Boolean] =
      state.tryRead.map(_.map(_.fold(true, _ => false, _ => false)).getOrElse(false))

    private[playground] val finalizing: PureConc[E, Boolean] =
      finalizers.tryRead.map(_.isEmpty)

    private[playground] val cancelImmediate: PureConc[E, Boolean] = {
      val checkM = withCtx[E, Boolean](_.masks.isEmpty.pure[PureConc[E, ?]])
      checkM.ifM(state.tryPut(Outcome.Canceled), false.pure[PureConc[E, ?]])
    }

    private[playground] def pushFinalizer(f: Finalizer[E]): PureConc[E, Unit] =
      finalizers.take.flatMap(fs => finalizers.put(f :: fs))

    private[playground] val popFinalizer: PureConc[E, Unit] =
      finalizers.take.flatMap(fs => finalizers.put(fs.drop(1)))

    private[playground] val cancelB: PureConc[E, Boolean] =
      cancelImmediate.ifM(
        (finalizers.take.flatMap(_.traverse_(_(Outcome.Canceled))) >> finalizers.put(Nil)).as(true),   // block simultaneous cancels
        false.pure[PureConc[E, ?]])

    val cancel: PureConc[E, Unit] = cancelB.void

    val join: PureConc[E, Outcome[PureConc[E, ?], E, A]] =
      state.read
  }
}
