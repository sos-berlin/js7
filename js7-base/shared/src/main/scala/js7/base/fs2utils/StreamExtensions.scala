package js7.base.fs2utils

import cats.effect.Concurrent
import cats.effect.{Resource, Sync}
import fs2.Stream
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*

object StreamExtensions:

  extension[F[_], A, B >: A](b: B)
    def +:(stream: Stream[F, A]): Stream[F, B] =
      stream.prepend(b)

  extension[F[_], A](stream: Stream[F, A])
    //def +:[A1 >: A](a: A1): Stream[F, A1] =
    //  prepend(a)

    def prepend[A1 >: A](a: A1): Stream[F, A1] =
      Stream.emit[F, A1](a) ++ stream

    def tapEach(f: A => Unit)(using F: Sync[F]): Stream[F, A] =
      stream.evalMap(a => F.delay:
        f(a)
        a)

    def takeUntilEval[X](completed: F[X])(using Concurrent[F]): Stream[F, A] =
      stream
        .map(Right(_))
        .merge:
          Stream.eval(completed).as(Left(()))
        .takeWhile(_.isRight)
        .map(_.asInstanceOf[Right[Unit, A]].value)
