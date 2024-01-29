package js7.base.monixlike

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Concurrent, GenSpawn, IO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.syntax.parallel.*
import cats.{ApplicativeError, Functor, MonadError}
import fs2.{Pull, Stream}
import js7.base.catsutils.CatsEffectExtensions.fromFutureWithEC
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.time.ScalaTime.*
import js7.base.utils.{CancelableFuture, emptyRunnable}
import scala.collection.Factory
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.{Failure, Success, Try}

object MonixLikeExtensions:

  extension (scheduler: Scheduler)
    def nowAsDeadline(): Deadline =
      Deadline(scheduler.monotonicNanos().ns)

    def scheduleOnce(after: FiniteDuration)(callback: => Unit)
      (using sourcecode.FullName)
    : SyncCancelable =
      SyncCancelable:
        scheduler.sleep(after, () => callback)

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
                scheduler.sleep((t - scheduler.monotonicNanos()).ns, callback)

      _currentCancelable = scheduler.sleep(delay, callback)
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
      underlying.handleErrorWith(throwable =>
        onError(
          throwable,
          initial,
          state => underlying.onErrorRestartLoop(state)(onError)))

    def raceFold[B >: A, E](canceler: F[B])(using F: GenSpawn[F, E] & Functor[F]): F[B] =
      F.race(canceler, underlying)
        .map(_.merge)

    //@deprecated("Use onError")
    //def tapError(tap: Throwable => Unit)(using F: ApplicativeError[F, Throwable], FM: FlatMap[F]): F[A] =
    //  underlying.onError(PartialFunction.fromFunction(F.delay(tap)))

    // For compatibility with Monix
    def materialize(using MonadError[F, Throwable]): F[Try[A]] =
      underlying.attempt.map(_.toTry)

    // For compatibility with Monix
    def dematerialize[A1](using F: MonadError[F, Throwable], ev: A =:= Try[A1]): F[A1] =
      underlying.asInstanceOf[F[Try[A1]]].flatMap:
        case Failure(t) => F.raiseError(t)
        case Success(a) => F.pure(a)

    @deprecated("NOT IMPLEMENTED")
    def onCancelRaiseError(throwable: => Throwable): F[A] =
      ???


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


  extension (x: IO.type)
    @deprecated("Monix compatibility")
    def parMap2[A, B, C](a: IO[A], b: IO[B])(f: (A, B) => C): IO[C] =
      IO.both(a, b).map((a, b) => f(a, b))

    @deprecated("Use IO.both")
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

    //@deprecated("Use more Cats-like fromFutureWithEC", "v2.7")
    def deferFutureAction[A](future: ExecutionContext => Future[A]): IO[A] =
      IO.fromFutureWithEC(ec => IO(future(ec)))


  extension [F[_], A](stream: Stream[F, A])

    @deprecated("Use onStart")
    inline def doOnSubscribe(onSubscribe: F[Unit]): Stream[F, A] =
      stream.onStart(onSubscribe)

    @deprecated("Check this call!")
    inline def doAfterSubscribe(afterSubscribe: F[Unit]): Stream[F, A] =
      stream.onStart(afterSubscribe)

    def takeUntilEval[X](completed: F[X])(using Concurrent[F]): Stream[F, A] =
      takeUntil(Stream.eval(completed))

    // TODO use interruptWhen
    def takeUntil[X](completed: Stream[F, X])(using Concurrent[F]): Stream[F, A] =
      stream
        .map(Right(_))
        .merge:
          completed.as(Left(()))
        .takeWhile(_.isRight)
        .map(_.asInstanceOf[Right[Unit, A]].value)

    def headL(using fs2.Compiler[F, F])(using Functor[F]): F[A] =
      stream.head.compile.last
        .map(_.getOrElse(throw new NoSuchElementException(".headL on empty stream")))

    def lastL(using fs2.Compiler[F, F])(using Functor[F]): F[A] =
      stream.last.compile.last
        .map(_.flatten)
        .map(_.getOrElse(throw new NoSuchElementException(".lastL on empty stream")))

    def toListL(using fs2.Compiler[F, F])(using Functor[F]): F[List[A]] =
      stream.compile.toList

    def completedL(using fs2.Compiler[F, F])(using Functor[F]): F[Unit] =
      stream.compile.drain


  extension [A](stream: Stream[IO, A])

    // Implementation taken from Stream.Pull.timeout documentation.
    def timeoutOnSlowUpstream(timeout: FiniteDuration): Stream[IO, A] =
      stream.pull
        .timed: timedPull =>
          def timeoutException = new UpstreamTimeoutException(
            s"timeoutOnSlowUpstream timed-out after ${timeout.pretty}")

          def go(timedPull: Pull.Timed[IO, A]): Pull[IO, A, Unit] =
            timedPull.timeout(timeout) >> // starts new timeout and stops the previous one
              timedPull.uncons.flatMap:
                case Some((Left(_), next)) => Pull.raiseError(timeoutException) >> go(next)
                case Some((Right(chunk), next)) => Pull.output(chunk) >> go(next)
                case None => Pull.done
          go(timedPull)
        .stream

  final class UpstreamTimeoutException(msg: String) extends TimeoutException(msg)
