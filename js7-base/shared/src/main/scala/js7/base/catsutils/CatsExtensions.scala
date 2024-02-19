package js7.base.catsutils

import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{ApplicativeError, MonadError}
import scala.util.{Failure, Success, Try}

object CatsExtensions:

  extension [F[_], A](underlying: F[A])

    /** Like attempt, but returns a Try. */
    def tryIt(using ApplicativeError[F, Throwable]): F[Try[A]] =
      underlying.attempt.map(_.toTry)

    /** Like rethrow, but from a Try. */
    def untry[A1](using F: MonadError[F, Throwable], ev: A =:= Try[A1]): F[A1] =
      underlying.asInstanceOf[F[Try[A1]]].flatMap:
        case Failure(t) => F.raiseError(t)
        case Success(a) => F.pure(a)
