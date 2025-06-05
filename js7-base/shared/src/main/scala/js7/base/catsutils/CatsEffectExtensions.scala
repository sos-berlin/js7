package js7.base.catsutils

import cats.effect.Resource.ExitCase
import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Clock, Fiber, FiberIO, IO, MonadCancel, Outcome, OutcomeIO, Resource, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Defer, Functor}
import js7.base.catsutils.CatsEffectUtils.{FiberCanceledException, outcomeToEither}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.NonFatalInterruptedException
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unused
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

object CatsEffectExtensions:

  // Must be lazy when a non-logging function is called before proper logger initialization.
  private lazy val logger = Logger[this.type]

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
    inline def adHocInfo(inline toMsg: A => String): IO[A] =
      io.flatTap(a => IO(logger.info(toMsg(a))))

    // inline for proper processing of Enclosing
    inline def adHocInfo(inline msg: String): IO[A] =
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

    /** Extra name to detect unjoined fibers in code. */
    def startAndForget(using enc: sourcecode.Enclosing): IO[Unit] =
      startAndForget(s"${enc.value} startAndForget")

    /** Extra name to detect unjoined fibers in code. */
    def startAndForget(label: => String): IO[Unit] =
      io.startAndCatchError.void

    def startAndLogError(using enc: sourcecode.Enclosing): IO[FiberIO[A]] =
      startAndLogError(s"${enc.value} startAndLogError")

    /** Extra name to detect unjoined fibers in code. */
    def startAndLogError(label: => String): IO[FiberIO[A]] =
      io.handleErrorWith: t =>
        logger.error(s"$label: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        IO.raiseError(t)
      .start

    def startAndCatchError(using enc: sourcecode.Enclosing): IO[FiberIO[Unit]] =
      startAndCatchError(s"${enc.value} startAndCatchError")

    /** Extra name to detect unjoined fibers in code. */
    def startAndCatchError(label: => String): IO[FiberIO[Unit]] =
      io.map:
        case Left(problem: Problem) => logger.error(s"$label: $problem")
        case _ =>
      .handleError: t =>
        logger.error(s"$label: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      .void
      .start

    def cancelWhen(trigger: IO[Unit]): IO[A] =
      IO
        .race(trigger, io)
        .flatMap:
          case Left(()) => IO.canceled.asInstanceOf[IO[A]]
          case Right(a) => IO.pure(a)

    ///** cancel operation should not fail. */
    //def onCancellation(onCancel: IO[Unit]): IO[A] =
    // io.start.flatMap: fiber =>
    //   fiber.join.flatMap(IO.fromOutcome).onCancel(onCancel)

    /** Use this function to compute the Throwable anew for each execution. */
    def timeoutAndFail(duration: Duration)(throwable: => Throwable): IO[A] =
      io.timeoutDefer(duration):
        IO.raiseError(throwable)

    /** Like Cats timeoutTo, but the fallback is computed anew for each execution. */
    def timeoutDefer[A1 >: A](duration: Duration)(fallback: => IO[A1]): IO[A1] =
      io.timeoutTo(duration, IO.defer(fallback))


  private val fromFutureDummyCancel = IO(logger.trace("fromFutureDummyCancelable ignores cancel"))

  extension [A](io: IO[Checked[A]])

    /** Catches a Throwable into `Checked[A]`. */
    // TODO Use catchIntoChecked
    def materializeIntoChecked: IO[Checked[A]] =
      catchIntoChecked

    /** Catches a Throwable into `Checked[A]`. */
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
  private val rightUnitIO = IO.pure(Right(()))

  extension(x: IO.type)
    def blockingOn[A](executionContext: ExecutionContext)(body: => A): IO[A] =
      IO:
        try
          val a = body
          IO.pure(a)
        catch case t: InterruptedException =>
          // Catch InterruptedException, because NonFatal considers it as fatal
          IO.raiseError(NonFatalInterruptedException(t))
      .evalOn(executionContext)
      .flatten

    def left[L](value: L): IO[Either[L, Nothing]] =
      IO.pure(Left(value))

    def right[R](value: R): IO[Either[Nothing, R]] =
      IO.pure(Right(value))

    inline def right(@unused u: Unit): IO[Either[Nothing, Unit]] =
      rightUnitIO

    inline def True: IO[Boolean] =
      trueIO

    inline def False: IO[Boolean] =
      falseIO

    inline def completed: IO[Completed] =
      completedIO

    def fromOutcome[A](outcome: OutcomeIO[A]): IO[A] =
      IO.fromEither(outcomeToEither(outcome)).flatten

    def fromFutureDummyCancelable[A](future: IO[Future[A]]): IO[A] =
      IO.fromFutureCancelable(future.map(_ -> fromFutureDummyCancel))

    def fromFutureWithEC[A](io: ExecutionContext => IO[Future[A]]): IO[A] =
      for
        ec <- IO.executionContext
        a <- IO.fromFuture(io(ec))
      yield a

    /** Like racePair but resolves the returned OutcomeIO[x]. */
    def raceBoth[L, R](left: IO[L], right: IO[R]): IO[Either[(L, FiberIO[R]), (FiberIO[L], R)]] =
      def resolve[A](outcome: OutcomeIO[A]): IO[A] =
        outcome.match
          case Outcome.Succeeded(io) => io
          case o => IO.fromOutcome(o)
      IO.racePair(left, right).flatMap:
        case Left((l, rFiber)) => resolve(l).map(l => Left(l -> rFiber))
        case Right((lFiber, r)) => resolve(r).map(r => Right(lFiber -> r))

    /** Like Cats raiseError but computes throwable anew for each execution. */
    def raiseError_[A](throwable: => Throwable): IO[A] =
      IO.defer(IO.raiseError((throwable)))

    def unsafeScheduler: IO[Scheduler] =
      IO.unsafeRuntime.map(_.scheduler)

    def unsafeRuntime: IO[IORuntime] =
      IO.executionContext.map(OurIORuntimeRegister.toIORuntime)

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
    def joinStd(using F: MonadCancel[F, Throwable] & Defer[F]): F[A] =
      fiber.joinWith(F.defer(F.raiseError(new FiberCanceledException)))

    def joinCatchError(label: => String)(using F: MonadCancel[F, Throwable] & Sync[F]): F[Unit] =
      fiber.join.flatMap:
        case Outcome.Succeeded(result) =>
          result.flatMap:
            case Left(problem: Problem) =>
              F.delay(logger.error(problem.toString, problem.throwableOption.orNull))
            case Left(t: Throwable) =>
              F.delay(logger.error(s"$label: ${t.toStringWithCauses}", t.nullIfNoStackTrace))
            case _ =>
              F.unit

        case Outcome.Errored(t) =>
          F.delay(logger.error(s"$label: ${t.toStringWithCauses}", t.nullIfNoStackTrace))

        case Outcome.Canceled() =>
          F.delay(logger.error(s"◼️ $label: Canceled"))


  extension [F[_], A](resource: Resource[F, Option[A]])
    def orIfNone(alternative: Resource[F, A]): Resource[F, A] =
      resource.flatMap:
        case None => alternative
        case Some(a) => Resource.pure(a)


  extension(x: Resource.type)

    def defer[F[_], A](resource: => Resource[F, A])(using F: Sync[F]): Resource[F, A] =
      Resource.suspend(F.delay(resource))

    def makeCancelable[F[_], A](acquire: F[A])(release: A => F[Unit])
      (using F: Functor[F])
    : Resource[F, A] =
      Resource.makeFull[F, A](poll => poll(acquire))(release)

    def makeCaseCancelable[F[_], A](acquire: F[A])(release: (A, ExitCase) => F[Unit])
      (using F: Functor[F])
    : Resource[F, A] =
      Resource.makeCaseFull[F, A](poll => poll(acquire))(release)

    def fromAutoCloseableCancelable[F[_], A <: AutoCloseable](acquire: F[A])(using F: Sync[F])
    : Resource[F, A] =
      makeCancelable(acquire)(a => F.blocking(a.close()))


  extension[F[_]](clock: Clock[F])
    def monotonicTime(using Functor[F]): F[CatsDeadline] =
      clock.monotonic.map(CatsDeadline.fromMonotonic)


  extension(scheduler: Scheduler)
    def now(): SyncDeadline =
      SyncDeadline.fromNanos(scheduler.monotonicNanos())
