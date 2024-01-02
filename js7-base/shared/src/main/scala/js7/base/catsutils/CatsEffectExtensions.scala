package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{Clock, GenSpawn, IO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.{ApplicativeError, Functor, MonadError}
import js7.base.log.Logger
import js7.base.utils.CancelableFuture
import scala.util.{Failure, Success, Try}

object CatsEffectExtensions:

  extension[F[_], A](underlying: F[A])
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


  extension[A](io: IO[A])
    def onErrorTap(pf: PartialFunction[Throwable, IO[Unit]]): IO[A] =
      io.attemptTap:
        case Right(_) => IO.unit
        case Left(t) => pf.applyOrElse(t, _ => IO.unit)

    def unsafeToCancelableFuture()(using IORuntime): CancelableFuture[A] =
      CancelableFuture(io.unsafeToFutureCancelable())

    inline def adHocInfo(inline toMsg: A => String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(Logger.info(toMsg(a))))

    inline def adHocInfo(inline msg: String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(Logger.info(msg)))


  extension(x: IO.type)
    def left[L](value: L): IO[Either[L, Nothing]] =
      IO.pure(Left(value))

    def right[R](value: R): IO[Either[Nothing, R]] =
      IO.pure(Right(value))


  //extension[F[_], E, A](fiber: Fiber[F, E, A])
  //  def joinStd(using F: MonadError[F, E]): F[A] =
  //    fiber.joinWith(F.defer(F.raiseError:
  //      new RuntimeException("Fiber has been canceled")))

  extension[F[_]](clock: Clock[F])
    def monotonicTime(using Functor[F]): F[CatsDeadline] =
      clock.monotonic.map(CatsDeadline(_))
