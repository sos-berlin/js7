package js7.base.catsutils

import cats.effect.IO
import cats.effect.kernel.GenSpawn
import cats.{:<:, Applicative, ApplicativeError, FlatMap, Functor, MonadError}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.syntax.applicativeError.*
import cats.syntax.applicative.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.{Failure, Success, Try}

object CatsEffectExtensions:

  extension[F[_], A](underlying: F[A])

    def onErrorRestartLoop[S, B >: A](initial: S)(onError: (Throwable, S, S => F[B]) => F[B])
      (using ApplicativeError[F, Throwable])
    : F[B] =
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
