package js7.base.fs2utils

import cats.effect.std.Queue
import cats.effect.{Concurrent, IO, Ref, Resource, Sync}
import cats.syntax.apply.*
import cats.syntax.option.*
import cats.{Eq, effect}
import fs2.{Chunk, Compiler, RaiseThrowable, Stream}
import js7.base.time.ScalaTime.RichDeadline
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}

object StreamExtensions:

  extension (chunk: Chunk[Char])
    def convertToString: String =
      chunk match
        case Chunk.ArraySlice(array, offset, length) =>
          String(array, offset, length)
        case _ =>
          String(chunk.toArray) // One extra copy


  extension (x: Chunk.type)
    def fromString(string: String): Chunk[Char] =
      Chunk.array(string.toCharArray)


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

    // TODO use interruptWhen
    def takeUntil[X](completed: Stream[F, X])(using Concurrent[F]): Stream[F, A] =
      stream
        .map(Right(_))
        .merge:
          completed.as(Left(()))
        .takeWhile(_.isRight)
        .map(_.asInstanceOf[Right[Unit, A]].value)

    /** When multiple elements are available, take only the newest one and drop the older ones.
     *
     * Consecutive equal elements are collapsed to one.
     */
    def onlyNewest(using Concurrent[F], Eq[A]): Stream[F, A] =
      for
        last <- Stream.eval(Ref[F].of(none[A]))
        output <-
          stream.chunks
            .flatMap(chunk => Stream.fromOption[F](chunk.last))
            .evalTap(a => last.set(Some(a)))
            .noneTerminate
            .hold1
            .flatMap(_.discrete)
            .unNoneTerminate
            .append(Stream.eval(last.get).unNoneTerminate)
            .changes
      yield output

    //def splitBigByteSeqs[ByteSeq: ByteSequence](chunkSize: Int)(using ByteSeq =:= A): Stream[F, ByteSeq] =
    //  stream.asInstanceOf[Stream[F, ByteSeq]]
    //    .flatMap: byteSeq =>
    //      if byteSeq.length <= chunkSize then
    //        Stream.emit(byteSeq)
    //      else
    //        def chunk(i: Int): (ByteSeq, Int) =
    //          (byteSeq.slice(i * chunkSize, (i + 1) * chunkSize), i + 1)
    //
    //        Stream.unfoldEval(0): i =>
    //          val pair = chunk(i)
    //          pair._1.nonEmpty ? pair

    /** Like IO recoverWith. */
    def recoverWith(pf: PartialFunction[Throwable, Stream[F, A]])(using F: RaiseThrowable[F])
    : Stream[F, A] =
      stream.handleErrorWith(pf orElse (t => Stream.raiseError[F](t)))

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

    def toListL(using Compiler[F, F]): F[List[A]] =
      stream.compile.toList


  extension[F[_]](stream: Stream[F, String])
    def stringToChar: Stream[F, Char] =
      stream.map(Chunk.fromString).unchunks


  extension[F[_]](stream: Stream[F, Char])
    def charToString: Stream[F, String] =
      stream.chunks.map(_.convertToString)


  extension[A](stream: Stream[IO, A])

    /** Mirror the source Stream as long as the source keeps emitting items,
     * otherwise if timeout passes without the source emitting anything new
     * then the Stream will start emitting the last item repeatedly.
     *
     * — <i>Taken from Monix Observable</i>
     */
    def echoRepeated(delay: FiniteDuration): Stream[IO, A] =
      for
        queue <- Stream.eval(Queue.bounded[IO, Option[Either[Throwable, Chunk[A]]]](capacity = 1))
        _ <- Stream.supervise:
          stream.chunks
            .filter(_.nonEmpty) // The processing below requires non-empty chunks
            .attempt
            .enqueueNoneTerminated(queue)
            .compile.drain
        maybeFirst <- Stream.eval(queue.take)
        result <- maybeFirst match
          case None => Stream.empty // End
          case Some(Left(error)) => Stream.raiseError[IO](error)
          case Some(Right(firstChunk)) =>
            for
              // Above we assured that firstChunk.isNonEmpty
              lastRef <- Stream.eval(Ref.of[IO, A](firstChunk.last.get))
              a <- Stream
                .chunk(firstChunk)
                .append(Stream
                  .eval:
                    queue.take
                      .timeoutTo(delay, lastRef.get.map(last => Some(Right(Chunk(last)))))
                      .flatTap(_
                        .flatMap(_.toOption)
                        .flatMap(_.last)
                        .fold(IO.none)(lastRef.set))
                  .repeat
                  .unNoneTerminate
                  .rethrow
                  .unchunks)
            yield a
      yield result

    def insertHeartbeatsOnSlowUpstream(delay: FiniteDuration, heartbeat: A): Stream[IO, A] =
      for
        queue <- Stream.eval:
          Queue.bounded[IO, Option[Either[Throwable, Chunk[A]]]](capacity = 1)
        _ <- Stream.supervise:
          stream.chunks.attempt.enqueueNoneTerminated(queue).compile.drain
        a <-
          Stream
            .eval:
              queue.take.timeoutTo(delay, IO.some(Right(Chunk(heartbeat))))
            .repeat
            .unNoneTerminate
            .rethrow
            .unchunks
      yield a


  extension(x: Stream.type)
    /** Like Monix Observable fromAsyncStateAction. */
    @deprecated("Use unfold")
    def fromAsyncStateAction[S, A](f: S => IO[(A, S)])(seed: => S): Stream[IO, A] =
      Stream.unfoldEval(seed)(s => f(s).map(Some(_)))


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
