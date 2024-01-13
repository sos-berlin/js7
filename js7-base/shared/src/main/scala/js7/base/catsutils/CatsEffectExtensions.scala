package js7.base.catsutils

import cats.effect.kernel.MonadCancel
import cats.effect.{Clock, Fiber, IO, Outcome}
import cats.syntax.functor.*
import cats.{Defer, Functor, MonadError, effect}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked
import scala.concurrent.{ExecutionContext, Future}

object CatsEffectExtensions:

  extension[A](io: IO[A])

    // inline for proper processing of Enclosing
    inline def adHocInfo(inline toMsg: A => String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(Logger.info(toMsg(a))))

    // inline for proper processing of Enclosing
    inline def adHocInfo(inline msg: String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(Logger.info(msg)))

    /** Converts a failed IO into a `Checked[A]`. */
    def catchAsChecked: IO[Checked[A]] =
      io.attempt.map(Checked.fromThrowableEither)


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

    def guaranteeExceptWhenRight(release: IO[Unit]): IO[Checked[A]] =
      io
        .guaranteeCase:
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

    @deprecated("Use more Cats-like fromFutureWithEC", "v2.7")
    def deferFutureAction[A](future: ExecutionContext => Future[A]): IO[A] =
      fromFutureWithEC(ec => IO(future(ec)))

    def fromFutureWithEC[A](io: ExecutionContext => IO[Future[A]]): IO[A] =
      for
        ec <- IO.executionContext
        a <- IO.fromFuture(io(ec))
      yield a


  extension[F[_], A](fiber: Fiber[F, Throwable, A])
    def joinStd(using F: MonadCancel[F, Throwable] & MonadError[F, Throwable] & Defer[F])
    : F[A] =
      fiber.joinWith(F.defer(F.raiseError:
        new RuntimeException("Fiber has been canceled")))


  extension[F[_]](clock: Clock[F])
    def monotonicTime(using Functor[F]): F[CatsDeadline] =
      clock.monotonic.map(CatsDeadline.fromMonotonic)
