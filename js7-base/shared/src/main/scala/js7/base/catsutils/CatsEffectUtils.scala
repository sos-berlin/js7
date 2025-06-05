package js7.base.catsutils

import cats.effect.{IO, Outcome, Sync}
import cats.syntax.flatMap.*
import scala.concurrent.{CancellationException, Promise}

object CatsEffectUtils:

  def whenDeferred[F[_]](condition: => Boolean)(body: => F[Unit])(using F: Sync[F]): F[Unit] =
    whenDeferred(F.delay(condition)):
      body

  def whenDeferred[F[_]](condition: F[Boolean])(body: => F[Unit])(using F: Sync[F]): F[Unit] =
    condition.flatMap: condition =>
      F.whenA(condition):
        body

  def unlessDeferred[F[_]](condition: => Boolean)(body: => F[Unit])(using F: Sync[F]): F[Unit] =
    unlessDeferred(F.delay(condition)):
      body

  def unlessDeferred[F[_]](condition: F[Boolean])(body: => F[Unit])(using F: Sync[F]): F[Unit] =
    condition.flatMap: condition =>
      F.unlessA(condition):
        body

  //def whenDeferred(condition: => Boolean)(body: => F[A])(using F: Sync[F], A: Monoid[A]): F[A] =
  //  F.defer:
  //    if condition then
  //      body
  //    else
  //      F.pure(A.empty)
  //
  //def unlessDeferred(condition: => Boolean)(body: => F[A])(using F: Sync[F], A: Monoid[A]): F[A] =
  //  whenDeferred(!condition)(body)

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


  final class FiberCanceledException extends CancellationException("Fiber has been canceled")
