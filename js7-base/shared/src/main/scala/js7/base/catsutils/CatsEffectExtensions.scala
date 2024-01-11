package js7.base.catsutils

import cats.effect.{Clock, IO}
import cats.syntax.functor.*
import cats.{Functor, effect}
import js7.base.log.Logger
import scala.concurrent.{ExecutionContext, Future}

object CatsEffectExtensions:

  extension[A](io: IO[A])

    inline def adHocInfo(inline toMsg: A => String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(Logger.info(toMsg(a))))

    inline def adHocInfo(inline msg: String)(using src: sourcecode.Enclosing): IO[A] =
      io.flatTap(a => IO(Logger.info(msg)))

  private val trueIO = IO.pure(true)
  private val falseIO = IO.pure(false)

  extension(x: IO.type)
    def left[L](value: L): IO[Either[L, Nothing]] =
      IO.pure(Left(value))

    def right[R](value: R): IO[Either[Nothing, R]] =
      IO.pure(Right(value))

    inline def True: IO[Boolean] =
      trueIO

    inline def False: IO[Boolean] =
      falseIO

    @deprecated("Use more Cats-like fromFutureWithEC", "v2.7")
    def deferFutureAction[A](future: ExecutionContext => Future[A]): IO[A] =
      fromFutureWithEC(ec => IO(future(ec)))

    def fromFutureWithEC[A](io: ExecutionContext => IO[Future[A]]): IO[A] =
      for
        ec <- IO.executionContext
        a <- IO.fromFuture(io(ec))
      yield a

  //extension[F[_], E, A](fiber: Fiber[F, E, A])
  //  def joinStd(using F: MonadError[F, E]): F[A] =
  //    fiber.joinWith(F.defer(F.raiseError:
  //      new RuntimeException("Fiber has been canceled")))


  extension[F[_]](clock: Clock[F])
    def monotonicTime(using Functor[F]): F[CatsDeadline] =
      clock.monotonic.map(CatsDeadline.fromMonotonic)

