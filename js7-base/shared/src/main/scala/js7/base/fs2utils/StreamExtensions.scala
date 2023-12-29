package js7.base.fs2utils

import cats.effect.{Concurrent, Sync}
import cats.syntax.apply.*
import cats.syntax.monadError.*
import fs2.Stream

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
      takeUntil(Stream.eval(completed))

    def takeUntil[X](completed: Stream[F, X])(using Concurrent[F]): Stream[F, A] =
      stream
        .map(Right(_))
        .merge:
          completed.as(Left(()))
        .takeWhile(_.isRight)
        .map(_.asInstanceOf[Right[Unit, A]].value)

    /** Like Monix Observable doOnSubscribe. */
    inline def doOnSubscribe(onSubscribe: F[Unit]): Stream[F, A] =
      onStart(onSubscribe)

    def onErrorEvalTap(pf: PartialFunction[Throwable, F[Unit]])(using F: Sync[F]): Stream[F, A] =
      stream.handleErrorWith(t =>
        Stream.eval:
          pf.applyOrElse(t, _ => F.unit) *> F.raiseError(t))

    /** Like Monix Observable doOnSubscribe. */
    def onStart(onStart: F[Unit]): Stream[F, A] =
      Stream.exec(onStart) ++ stream
