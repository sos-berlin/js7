package js7.base.catsutils

import cats.Monoid
import cats.effect.{IO, Outcome, Sync}
import cats.syntax.flatMap.*
import cats.syntax.monoid.*
import scala.concurrent.{CancellationException, Promise}

object CatsEffectUtils:

  def whenDeferred[F[_]](condition: => Boolean)(body: => F[Unit])(using F: Sync[F]): F[Unit] =
    whenDeferred(F.delay(condition)):
      body

  def whenDeferred[F[_]](condition: F[Boolean])(body: => F[Unit])(using F: Sync[F]): F[Unit] =
    condition.flatMap: condition =>
      F.whenA(condition):
        body

  def outcomeToEither[F[_], A](outcome: Outcome[F, Throwable, A]): Either[Throwable, F[A]] =
    outcome match
      case Outcome.Succeeded(a) => Right(a)
      case Outcome.Errored(e) => Left(e)
      case Outcome.Canceled() => Left(new FiberCanceledException)

  def promiseIO[A](body: Promise[A] => Unit): IO[A] =
    IO.defer:
      val promise = Promise[A]()
      body(promise)
      IO.fromFuture(IO.pure(promise.future))


  given UnitIOMonoid: Monoid[IO[Unit]] =
    new Monoid[IO[Unit]]:
      val empty: IO[Unit] = IO.unit

      def combine(a: IO[Unit], b: IO[Unit]): IO[Unit] =
        a *> b

  given ioMonoid[A: Monoid as A]: Monoid[IO[A]] =
    new Monoid[IO[A]]:
      val empty: IO[A] = IO.pure(A.empty)

      def combine(a: IO[A], b: IO[A]): IO[A] =
        for
          a <- a
          b <- b
        yield
          a |+| b

  final class FiberCanceledException(message: String = "Fiber has been canceled")
  extends CancellationException(message):
    override def toString: String = s"FiberCanceledException: $message"
