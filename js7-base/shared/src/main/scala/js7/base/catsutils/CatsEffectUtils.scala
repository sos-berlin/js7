package js7.base.catsutils

import cats.effect.{IO, Outcome}
import scala.concurrent.{CancellationException, Promise}

object CatsEffectUtils:

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

