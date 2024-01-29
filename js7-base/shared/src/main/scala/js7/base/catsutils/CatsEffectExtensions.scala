package js7.base.catsutils

import cats.effect.kernel.MonadCancel
import cats.effect.unsafe.Scheduler
import cats.effect.{Clock, Fiber, IO, Outcome, OutcomeIO}
import cats.syntax.functor.*
import cats.{Defer, Functor, effect}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.{ExecutionContext, Future}

object CatsEffectExtensions:

  private val logger = Logger[this.type]

  extension[A](io: IO[A])

    /** Evaluates the Canceled() case only when not Canceled. */
    def guaranteeCaseLazy(finalizer: OutcomeIO[A @uncheckedVariance] => IO[Unit]): IO[A] =
      io.guaranteeCase:
        case o @ Outcome.Canceled() => IO.defer(finalizer(o))
        case o => finalizer(o)

    /** Evaluates the Canceled() only when not canceled. */
    def onCancelLazy(fin: => IO[Unit]): IO[A] =
      io.onCancel(IO.defer(fin))

    // inline for proper processing of Enclosing
    inline def adHocInfo(inline toMsg: A => String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(logger.info(toMsg(a))))

    // inline for proper processing of Enclosing
    inline def adHocInfo(inline msg: String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(logger.info(msg)))

    /** Converts a failed IO into a `Checked[A]`. */
    def catchAsChecked: IO[Checked[A]] =
      io.attempt.map(Checked.fromThrowableEither)

    @deprecated("Use guaranteeExceptWhenSucceeded")
    def guaranteeExceptWhenCompleted(finalizer: IO[Unit]): IO[A] =
      guaranteeExceptWhenSucceeded(finalizer)

    def guaranteeExceptWhenSucceeded(finalizer: IO[Unit]): IO[A] =
      io.guaranteeCase:
        case Outcome.Succeeded(_) => IO.unit
        case _ => finalizer

    def startAndForget: IO[Unit] =
      io.start.void


  extension [A](io: IO[Checked[A]])

    /** Catchs a Throwable into `Checked[A]`. */
    @deprecated("Use catchIntoChecked")
    def materializeIntoChecked: IO[Checked[A]] =
      catchIntoChecked

    /** Catchs a Throwable into `Checked[A]`. */
    def catchIntoChecked: IO[Checked[A]] =
      io.attempt.map(either => Checked.fromThrowableEither(either).flatten)

    def orThrow: IO[A] =
      io.flatMap:
        case Left(problem) => IO.raiseError(problem.throwable)
        case Right(a) => IO.pure(a)

    def guaranteeExceptWhenRight(release: => IO[Unit]): IO[Checked[A]] =
      io
        .guaranteeCaseLazy:
          case Outcome.Succeeded(_) => IO.unit
          case _ => release
        .flatTap:
          case Left(problem) => release.as(Left(problem))
          case Right(_) => IO.unit


  private val trueIO = IO.pure(true)
  private val falseIO = IO.pure(false)
  private val completedIO = IO.pure(Completed)

  extension(x: IO.type)
    def left[L](value: L): IO[Either[L, Nothing]] =
      IO.pure(Left(value))

    def right[R](value: R): IO[Either[Nothing, R]] =
      IO.pure(Right(value))

    inline def True: IO[Boolean] =
      trueIO

    inline def False: IO[Boolean] =
      falseIO

    inline def completed: IO[Completed] =
      completedIO

    def fromFutureWithEC[A](io: ExecutionContext => IO[Future[A]]): IO[A] =
      for
        ec <- IO.executionContext
        a <- IO.fromFuture(io(ec))
      yield a

    def unsafeScheduler: IO[Scheduler] =
      IO.unsafeRuntime.map(_.scheduler)

    //def fromCancelableFutureWithEC[A](io: ExecutionContext => IO[CancelableFuture[A]]): IO[A] =
    //  for
    //    ec <- IO.executionContext
    //    a <- IO.fromCancelableFuture(io(ec))
    //  yield a
    //
    //def fromCancelableFuture[A](io: IO[CancelableFuture[A]]): IO[A] =
    //  io.flatMap(future => IO.fromFutureCancelable(future, future.cancelToFuture)


  extension[F[_], A](fiber: Fiber[F, Throwable, A])
    /** Like joinWithUnit but a canceled Fiber results in a Throwable. */
    def joinStd(using F: MonadCancel[F, Throwable] & Defer[F])
    : F[A] =
      fiber.joinWith(F.defer(F.raiseError(new FiberCanceledException)))


  extension[F[_]](clock: Clock[F])
    def monotonicTime(using Functor[F]): F[CatsDeadline] =
      clock.monotonic.map(CatsDeadline.fromMonotonic)


  final class FiberCanceledException extends RuntimeException("Fiber has been canceled")
