package js7.base.fs2utils

import cats.effect
import cats.effect.{Concurrent, Resource, Sync}
import cats.syntax.apply.*
import fs2.{Chunk, RaiseThrowable, Stream}
import js7.base.time.ScalaTime.RichDeadline
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}

object StreamExtensions:

  extension[F[_], A, B >: A](b: B)
    def +:(stream: Stream[F, A]): Stream[F, B] =
      stream.prependOne(b)

  extension[F[_], A](stream: Stream[F, A])
    //def +:[A1 >: A](a: A1): Stream[F, A1] =
    //  prependOne(a)

    def prependOne[A1 >: A](a: A1): Stream[F, A1] =
      Stream.emit[F, A1](a) ++ stream

    def appendOne[A1 >: A](a: A1): Stream[F, A1] =
      stream ++ Stream.emit[F, A1](a)

    def tapEach(f: A => Unit)(using F: Sync[F]): Stream[F, A] =
      stream.evalMap(a => F.delay:
        f(a)
        a)

    def tapEachChunk(f: Chunk[A] => Unit)(using F: Sync[F]): Stream[F, A] =
      stream
        .chunks
        .evalMap(chunk => F.delay:
          f(chunk)
          chunk)
        .unchunks

    def takeUntilEval[X](completed: F[X])(using Concurrent[F]): Stream[F, A] =
      takeUntil(Stream.eval(completed))

    def takeUntil[X](completed: Stream[F, X])(using Concurrent[F]): Stream[F, A] =
      stream
        .map(Right(_))
        .merge:
          completed.as(Left(()))
        .takeWhile(_.isRight)
        .map(_.asInstanceOf[Right[Unit, A]].value)

    /** Like IO recoverWith. */
    def recoverWith(pf: PartialFunction[Throwable, Stream[F, A]])(using F: RaiseThrowable[F])
    : Stream[F, A] =
      stream.handleErrorWith(pf orElse (t => Stream.raiseError[F](t)))

    /** Like Monix Observable doOnSubscribe. */
    @deprecated("Use onStart")
    inline def doOnSubscribe(onSubscribe: F[Unit]): Stream[F, A] =
      onStart(onSubscribe)

    def onErrorEvalTap(pf: PartialFunction[Throwable, F[Unit]])(using F: Sync[F]): Stream[F, A] =
      stream.handleErrorWith(t =>
        Stream.eval:
          pf.applyOrElse(t, _ => F.unit) *> F.raiseError(t))

    /** Like Monix Observable doOnSubscribe. */
    def onStart(onStart: F[Unit]): Stream[F, A] =
      Stream.exec(onStart) ++ stream

    def logTiming(
      toCount: A => Long = simpleCount,
      onComplete: (FiniteDuration, Long, Resource.ExitCase) => Unit,
      startedAt: Deadline = now)
      (using F: Sync[F])
    : Stream[F, A] =
      Stream.suspend:
        var count = 0L
        stream
          .map: a =>
            count += toCount(a)
            a
          .onFinalizeCase: exitCase =>
            F.delay(onComplete(startedAt.elapsed, count, exitCase))

  //extension [A](underlying: IO[Stream[IO, A]])
  //  def logTiming(
  //    toCount: A => Long = simpleCount,
  //    onComplete: (FiniteDuration, Long, Outcome[IO, A, Throwable]) => Unit,
  //    startedAt: Deadline = now)
  //  : IO[Stream[IO, A]] =
  //    underlying
  //      .map(Right(_))
  //      .logTiming(toCount, onComplete, startedAt)
  //      .map(_.orThrow /*never throws*/)
  //
  //extension [A](underlying: IO[Checked[Stream[IO, A]]])
  //  def logTiming(
  //    toCount: A => Long = simpleCount,
  //    onComplete: (FiniteDuration, Long, Outcome[IO, A, Throwable]) => Unit,
  //    startedAt: => Deadline = now)
  //  : IO[Checked[Stream[IO, A]]] =
  //    IO.defer:
  //      val startedAt_ = startedAt
  //      var counter = 0L
  //      underlying.map(_.map(_
  //        .map: a =>
  //          counter += toCount(a)
  //          a
  //        .onFinalizeCase(exitCase => IO:
  //          onComplete(startedAt_.elapsed, counter, exitCase.toOutcome))))

  private def simpleCount[A](a: A) = 1L
