package js7.base.monixlike

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{GenSpawn, IO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.{ApplicativeError, Functor, MonadError}
import fs2.{Pull, Stream}
import js7.base.time.ScalaTime.*
import js7.base.utils.CancelableFuture
import scala.concurrent.TimeoutException
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.util.{Failure, Success, Try}

object MonixLikeExtensions:

  extension (scheduler: Scheduler)
    def nowAsDeadline(): Deadline =
      Deadline(scheduler.monotonicNanos().ns)

    def scheduleOnce(after: FiniteDuration)(callback: => Unit): Cancelable =
      Cancelable.fromRunnable:
        scheduler.sleep(after, () => callback)

    def scheduleAtFixedRate(delay: FiniteDuration, repeat: FiniteDuration)(body: => Unit)
    : Cancelable =
      val repeatNanos = repeat.toNanos
      var t = scheduler.monotonicNanos() + delay.toNanos

      object callback extends Runnable:
        def run() =
          try body
          finally
            t += repeatNanos
            scheduler.sleep((t - scheduler.monotonicNanos()).ns, callback)

      Cancelable.fromRunnable(scheduler.sleep(delay, callback))

    def scheduleAtFixedRates(durations: IterableOnce[FiniteDuration])(body: => Unit): Cancelable =
      val cancelable = SerialCancelable()
      val iterator = durations.iterator

      def loop(last: Deadline): Unit =
        val nextDuration = iterator.next()
        val next = last + nextDuration
        val delay = next - nowAsDeadline()
        cancelable := (
          if iterator.hasNext then
            scheduleOnce(delay):
              body
              loop(next)
          else
            scheduleAtFixedRate(delay, nextDuration)(body))

      if iterator.hasNext then
        loop(nowAsDeadline())

      cancelable


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
        .map(_.fold(identity, identity))

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


  extension [A](io: IO[A])
    def onErrorTap(pf: PartialFunction[Throwable, IO[Unit]]): IO[A] =
      tapError(pf)

    def tapError(pf: PartialFunction[Throwable, IO[Unit]]): IO[A] =
      io.attemptTap:
        case Right(_) => IO.unit
        case Left(t) => pf.applyOrElse(t, _ => IO.unit)

    def unsafeToCancelableFuture()(using IORuntime): CancelableFuture[A] =
      CancelableFuture.fromPair(io.unsafeToFutureCancelable())


  extension (x: IO.type)
    @deprecated("Monix compatibility")
    def parMap2[A, B, C](a: IO[A], b: IO[B])(f: (A, B) => C): IO[C] =
      IO.both(a, b).map((a, b) => f(a, b))

    @deprecated("Use IO.both")
    def parZip2[A, B](a: IO[A], b: IO[B]): IO[(A, B)] =
      IO.both(a, b)


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
