package js7.base.monixlike

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Concurrent, GenSpawn, IO}
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.syntax.parallel.*
import cats.{ApplicativeError, Functor, MonadError}
import fs2.Stream
import js7.base.catsutils.CatsExtensions.*
import js7.base.fs2utils.StreamExtensions.{interruptWhenF, onStart}
import js7.base.time.ScalaTime.*
import js7.base.utils.{CancelableFuture, emptyRunnable}
import scala.collection.Factory
import scala.concurrent.Future
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.util.Try

/*
 * ——— SOME PARTS ORIGINATE FROM MONIX ———
 *
 * Copyright (c) 2014-2021 by The Monix Project Developers.
 * See the project homepage at: https://monix.io
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
/** Some descriptions and some small code parts originate from Monix.
 *
 * @see https://github.com/monix/monix
 */
object MonixLikeExtensions:

  extension (scheduler: Scheduler)
    def nowAsDeadline(): Deadline =
      Deadline(scheduler.monotonicNanos().ns)

    def scheduleOnce(after: FiniteDuration)(callback: => Unit)
      (using sourcecode.FullName)
    : SyncCancelable =
      SyncCancelable:
        scheduler.sleep(after max ZeroDuration, () => callback)

    def scheduleAtFixedRate(delay: FiniteDuration, repeat: FiniteDuration)(body: => Unit)
    : SyncCancelable =
      val repeatNanos = repeat.toNanos
      var t = scheduler.monotonicNanos() + delay.toNanos
      @volatile var _stop = false
      @volatile var _currentCancelable = emptyRunnable

      object callback extends Runnable:
        def run() =
          if !_stop then
            try body
            finally
              t += repeatNanos
              _currentCancelable =
                scheduler.sleep((t - scheduler.monotonicNanos()).ns max ZeroDuration, callback)

      _currentCancelable = scheduler.sleep(delay max ZeroDuration, callback)
      SyncCancelable: () =>
        _stop = true
        _currentCancelable.run()

    def scheduleAtFixedRates(durations: IterableOnce[FiniteDuration])(body: => Unit)
    : SyncCancelable =
      var _stop = false
      var _currentCancelable = SyncCancelable.empty
      val iterator = durations.iterator

      def loop(last: Deadline): Unit =
        val nextDuration = iterator.next()
        val next = last + nextDuration
        val delay = next - nowAsDeadline()
        _currentCancelable =
          if iterator.hasNext then
            scheduleOnce(delay):
              if !_stop then
                try body
                finally loop(next)
          else
            scheduleAtFixedRate(delay, nextDuration)(body)

      if iterator.hasNext then
        loop(nowAsDeadline())

      SyncCancelable: () =>
        _stop = true
        _currentCancelable.cancel()


  extension [F[_], A](underlying: F[A])
    /** Like Monix. */
    def onErrorRestartLoop[S](initial: S)(onError: (Throwable, S, S => F[A]) => F[A])
      (using ApplicativeError[F, Throwable])
    : F[A] =
      underlying.handleErrorWith: throwable =>
        onError(
          throwable,
          initial,
          state => underlying.onErrorRestartLoop(state)(onError))

    def raceFold[B >: A, E](canceler: F[B])(using F: GenSpawn[F, E] & Functor[F]): F[B] =
      F.race(canceler, underlying)
        .map(_.merge)

    // For compatibility with Monix
    def materialize(using ApplicativeError[F, Throwable]): F[Try[A]] =
      underlying.tryIt

    // For compatibility with Monix
    def dematerialize[A1](using MonadError[F, Throwable], A =:= Try[A1]): F[A1] =
      underlying.untry


  extension [A](io: IO[A])
    def onErrorTap(pf: PartialFunction[Throwable, IO[Unit]]): IO[A] =
      tapError(pf)

    def tapError(pf: PartialFunction[Throwable, IO[Unit]]): IO[A] =
      io.attemptTap:
        case Right(_) => IO.unit
        case Left(t) => pf.applyOrElse(t, _ => IO.unit)

    def unsafeToCancelableFuture()(using IORuntime): CancelableFuture[A] =
      CancelableFuture.fromPair(io.unsafeToFutureCancelable())

    def foreach(f: A => Unit)(using IORuntime): Future[Unit] =
      io.flatMap(a => IO(f(a))).unsafeToFuture()

    /** Describes flatMap-driven loops, as an alternative to recursive functions.
     *
     * ©Monix
     *
     * Sample:
     *
     * {{{
     *   import scala.util.Random
     *
     *   val random = IO(Random.nextInt())
     *   val loop = random.flatMapLoop(Vector.empty[Int]) { (a, list, continue) =>
     *     val newList = list :+ a
     *     if (newList.length < 5)
     *       continue(newList)
     *     else
     *       IO.now(newList)
     *   }
     * }}}
     *
     * @param seed initializes the result of the loop
     * @param f    is the function that updates the result
     *             on each iteration, returning a `IO`.
     * @return a new [[IO]] that contains the result of the loop.
     *
     */
    def flatMapLoop[S](seed: S)(f: (A, S, S => IO[S]) => IO[S]): IO[S] =
      io.flatMap: a =>
        f(a, seed, flatMapLoop(_)(f))


  extension (x: IO.type)
    def parMap2[A, B, C](a: IO[A], b: IO[B])(f: (A, B) => C): IO[C] =
      IO.both(a, b).map((a, b) => f(a, b))

    def parZip2[A, B](a: IO[A], b: IO[B]): IO[(A, B)] =
      IO.both(a, b)

    def parSequence[A, M[X] <: Iterable[X]](in: M[IO[A]])
      (using factory: Factory[A, M[A]])
    : IO[M[A]] =
      in.toSeq.parSequence.map(_.to(factory))

    def parTraverse[A, B, M[X] <: Iterable[X]](in: M[A])
      (f: A => IO[B])
      (using factory: Factory[B, M[B]])
    : IO[M[B]] =
      in.toSeq.parTraverse(f).map(_.to(factory))

    def deferFuture[A](future: => Future[A]): IO[A] =
      IO.fromFuture(IO(future))


  extension [F[_], A](stream: Stream[F, A])

    @deprecated("Use onStart")
    inline def doOnSubscribe(onSubscribe: F[Unit]): Stream[F, A] =
      stream.onStart(onSubscribe)

    @deprecated("Check this call!")
    inline def doAfterSubscribe(afterSubscribe: F[Unit]): Stream[F, A] =
      stream.onStart(afterSubscribe)

    def takeUntilEval[F2[x] >: F[x]](haltOnCompletion: F2[Unit])
      (using ApplicativeError[F2, Throwable])
    : Stream[F2, A] =
      stream.interruptWhenF(haltOnCompletion)

    def takeUntil[X](other: Stream[F, X])(using Concurrent[F]): Stream[F, A] =
      stream.interruptWhen(other.as(true))

    def headL(using fs2.Compiler[F, F], Functor[F]): F[A] =
      stream.head.compile.last
        .map(_.getOrElse(throw new NoSuchElementException(".headL on empty stream")))

    def lastL(using fs2.Compiler[F, F], Functor[F]): F[A] =
      stream.last.compile.last
        .map(_.flatten)
        .map(_.getOrElse(throw new NoSuchElementException(".lastL on empty stream")))

    def toListL(using fs2.Compiler[F, F], Functor[F]): F[List[A]] =
      stream.compile.toList

    def completedL(using fs2.Compiler[F, F], Functor[F]): F[Unit] =
      stream.compile.drain
