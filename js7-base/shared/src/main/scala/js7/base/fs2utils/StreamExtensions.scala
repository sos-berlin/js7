package js7.base.fs2utils

import cats.effect.Resource.ExitCase
import cats.effect.std.{AtomicCell, Queue}
import cats.effect.{Concurrent, IO, Ref, Sync, Temporal}
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.{Applicative, ApplicativeError, Eq, effect}
import fs2.{Chunk, Compiler, Pull, RaiseThrowable, Stream}
import js7.base.ProvisionalAssumptions
import js7.base.time.ScalaTime.{RichDeadline, RichFiniteDurationCompanion}
import js7.base.utils.ScalaUtils.syntax.RichAny
import js7.base.utils.{Atomic, MultipleLinesBracket}
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.reflect.ClassTag

object StreamExtensions:

  val DefaultBatchSizeMin = 256
  private object Beat

  extension [A](chunk: Chunk[A])
    def grouped(size: Int): Iterator[Chunk[A]] =
      if size <= 0 then
        throw new IllegalArgumentException(s"Chunk.grouped size must be positive: $size")
      val myGroupSize = size // size is a name in Iterater superclass, too
      var remainder = chunk

      new Iterator[Chunk[A]]:
        def hasNext = remainder.nonEmpty

        def next() =
          if remainder.isEmpty then
            throw new NoSuchElementException("Chunk.grouped next but !hasNext")
          val (a, b) = remainder.splitAt(myGroupSize)
          remainder = b
          a

  extension (chunk: Chunk[Char])
    def convertToString: String =
      chunk match
        case Chunk.ArraySlice(array: Array[Char], offset, length) =>
          String(array, offset, length)
        case _ =>
          String(chunk.toArray) // One extra copy

  extension [A](chunk: Chunk[Chunk[A]])
    def flatten: Chunk[A] =
      chunk.flatMap(identity)


  extension (x: Chunk.type)
    def fromString(string: String): Chunk[Char] =
      Chunk.array[Char](string.toCharArray)


  extension[F[_], A, B >: A](b: B)
    def +:(stream: Stream[F, A]): Stream[F, B] =
      stream.prependOne(b)


  extension[F[_], O](stream: Stream[F, O])
    /*inline not usable here, in Scala 3.5.2*/
    def :+[O1 >: O](o: => O1): Stream[F, O1] =
      stream.appendOne(o)

    /** Like ++, but has same operator priority as `:+`. */
    inline def :++(inline other: Stream[F, O]): Stream[F, O] =
      stream.append(other)

    /** Like FS2's keepAlive, but adds only a single beat after maxIdle has elapsed. */
    def addAfterIdle[F2[x] >: F[x]: Temporal, B](maxIdle: FiniteDuration, beat: F2[B])
    : Stream[F2, O | B] =
      stream
        .covaryAll[F2, O | B]
        .pull
        .timed: timedPull =>
          def go(timedPull: Pull.Timed[F2, O | B]): Pull[F2, O | B, Unit] =
            timedPull.uncons.flatMap:
              case Some((Right(chunk), next)) =>
                Pull.output(chunk)
                  >> timedPull.timeout(maxIdle - FiniteDuration.Epsilon)
                  >> go(next)
              case Some((_, next)) =>
                Pull.eval(beat).flatMap(Pull.output1) >> go(next)
              case None =>
                Pull.done

          go(timedPull)
        .stream

    def collectAndFlushOnSilence(duration: FiniteDuration)(using F: Temporal[F])
    : Stream[F, Chunk[O]] =
      Stream
        .eval(AtomicCell[F].of(VectorBuilder[O]()))
        .flatMap: cell =>
          stream
            .chunks
            .evalTap: chunk =>
              cell.getAndUpdate(_ ++= chunk.asSeq)
            // Emit a Beat after the required period of silence
            .addAfterIdle(duration - FiniteDuration.Epsilon, F.pure(Beat))
            // Add a Beat to the end to emit the remaining elements
            .appendOne(Beat)
            // Emit the Chunk[O] after the period of silence
            .flatMap:
              case Beat => Stream
                .eval(cell
                  .getAndSet(VectorBuilder())
                  .map(builder => Chunk.from(builder.result())))
                .filter(_.nonEmpty)
              case _ => Stream.empty

    /** Provisional to group elements into chunks, for better throughput. */
    def chunkProvisional(using Temporal[F]): Stream[F, O] =
      import ProvisionalAssumptions.streamChunks.elementsPerChunkLimit
      chunkProvisional(elementsPerChunkLimit)

    /** Provisional to group elements into chunks, for better throughput. */
    def chunkProvisional(chunkSize: Int)(using Temporal[F]): Stream[F, O] =
      import ProvisionalAssumptions.streamChunks.groupWithinDelay
      stream.groupWithin(chunkSize, groupWithinDelay).unchunks

    /** Similar to FS2 groupWithin.
     *
     * There was some unexpected behaviour with groupWithin, therefore an own implementation:
     * <ul>
     *   <li>The first chunk after a while is not split.
     *   <li>Always fills chunks, even with timeout == 0s.
     * </ul>
     */
    def chunkWithin(chunkSize: Int, timeout: FiniteDuration)(using F: Temporal[F])
    : Stream[F, Chunk[O]] =
      stream.pull
        .timed: timedPull =>
          def go(timedPull: Pull.Timed[F, O], queue: Chunk[O], queueSize: Int)
          : Pull[F, Chunk[O], Unit] =
            assert(queueSize == queue.size)
            timedPull.timeout(timeout) >>
              timedPull.uncons.flatMap:

                case Some((Right(chunk), next)) =>
                  val nextLength = queueSize + chunk.size
                  if nextLength < chunkSize then
                    go(next, queue ++ chunk, nextLength)
                  else
                    val chunks = Chunk.iterator((queue ++ chunk).grouped(chunkSize))
                    if chunks.last.size == chunkSize then
                      Pull.output(chunks) >> go(next, Chunk.empty, 0)
                    else
                      val nextQueue = chunks.last.get
                      Pull.output(chunks.dropRight(1)) >> go(next, nextQueue, nextQueue.size)

                case Some((Left(_), next)) =>
                  if queueSize == 0 then
                    go(next, Chunk.empty, 0)
                  else
                    Pull.output(Chunk.iterator(queue.grouped(chunkSize)))
                      >> go(next, Chunk.empty, 0)

                case None =>
                  if queueSize == 0 then
                    Pull.done
                  else
                    Pull.output(Chunk.iterator(queue.grouped(chunkSize)))
                      >> Pull.done

          go(timedPull, Chunk.empty, 0)
        .stream
        .filter(_.nonEmpty)

    /** Repeat endlessly the last element, or return the empty Stream iff this is empty. */
    def repeatLast: Stream[F, O] =
      Stream.suspend:
        val empty = new AnyRef // Unique value
        @volatile var last: O | empty.type = empty
        stream
          .map: o =>
            last = o
            o
          .append:
            last match
              case `empty` => Stream.empty // The Stream was empty
              case o: O @unchecked => Stream.constant(o).repeat
              //case o: O @unchecked => Stream.chunk(Chunk.from(Array.fill(chunkSize)(o))).repeat

    def prepend[F2[x] >: F[x], O2 >: O](prefix: Stream[F2, O2]): Stream[F2, O2] =
      prefix ++ stream

    def prependOne[O1 >: O](o: O1): Stream[F, O1] =
      prepend(Stream.emit[F, O1](o))

    def appendOne[O1 >: O](o: => O1): Stream[F, O1] =
      stream ++ Stream.emit[F, O1](o)

    def fillUpChunks(limit: Int): Stream[F, Chunk[O]] =
      stream.repeatPull:
        _.unconsMin(limit, allowFewerTotal = true).flatMap:
          case Some((hd, tl)) => Pull.output1(hd).as(Some(tl))
          case None           => Pull.pure(None)

    def onStart(onStart: F[Unit]): Stream[F, O] =
      Stream.exec(onStart) ++ stream

    def onFirst(onFirst: O => F[Unit])(using F: Sync[F]): Stream[F, O] =
      Stream.suspend:
        val used = Atomic(false)
        stream.evalTap: o =>
          F.unlessA(used.getAndSet(true)):
            onFirst(o)

    def tapEach(f: O => Unit)(using F: Sync[F]): Stream[F, O] =
      stream.evalMap(a => F.delay:
        f(a)
        a)

    def tapEachChunk(f: Chunk[O] => Unit)(using F: Sync[F]): Stream[F, O] =
      stream
        .chunks
        .evalMap(chunk => F.delay:
          f(chunk)
          chunk)
        .unchunks

    /** When multiple elements are available, take only the newest one and drop the older ones.
     * <ul>
     * <li>Take only the last element of each non-empty Chunk if
     * it differs from the last emitted element.
     * <li>Only non-empty Chunks are emitted.
     * </ul>
     * Consecutive equal elements are collapsed to one.
     */
    def onlyNewest(using Concurrent[F], Eq[O]): Stream[F, O] =
      for
        last <- Stream.eval(Ref[F].of(none[O]))
        output <-
          stream
            .mapChunks(chunk => Chunk.fromOption(chunk.last))
            .evalTap: o =>
              last.set(Some(o))
            .noneTerminate
            .hold1
            .flatMap(_.discrete)
            .unNoneTerminate
            .append:
              Stream.eval(last.get).unNoneTerminate
            .changes
      yield
        output

    //def splitBigByteSeqs[ByteSeq: ByteSequence](chunkSize: Int)(using ByteSeq =:= O): Stream[F, ByteSeq] =
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

    def updateStateWhileInclusive[S](seed: S)(predicate: S => Boolean)(f: (S, O) => S)
    : Stream[F, O] =
      updateState(seed)(f)
        .takeThrough(o => predicate(o._1))
        .map(_._2)

    def updateState[S](seed: S)(f: (S, O) => S): Stream[F, (S, O)] =
      stream
        .scan((seed, null.asInstanceOf[O])):
          case ((state, _), a) => f(state, a) -> a
        .drop(1) // Drop initial null dummy

    def onErrorEvalTap(pf: PartialFunction[Throwable, F[Unit]])(using F: Sync[F]): Stream[F, O] =
      stream.handleErrorWith(t =>
        Stream.eval(pf.applyOrElse(t, _ => F.unit))
          *> Stream.raiseError(t))

    def interruptWhenF[F2[x] >: F[x]](haltOnCompletion: F2[Unit])
      (using ApplicativeError[F2, Throwable])
    : Stream[F2, O] =
      stream.interruptWhen(haltOnCompletion.attempt)

    def logTiming(
      toCount: O => Long = simpleCount,
      onComplete: (FiniteDuration, Long, ExitCase) => F[Unit],
      startedAt: Deadline = now)
      (using Applicative[F])
    : Stream[F, O] =
      Stream.suspend:
        var count = 0L
        stream
          .map: a =>
            count += toCount(a)
            a
          .onFinalizeCase: exitCase =>
            onComplete(startedAt.elapsed, count, exitCase)

    def zipWithBracket(bracket: MultipleLinesBracket | String): Stream[F, (O, Char)] =
      val brckt = MultipleLinesBracket.normalize(bracket)
      stream.through:
        _.zipWithPreviousAndNext.map:
          case (None, o, Some(_)) => o -> brckt.first
          case (Some(_), o, Some(_)) => o -> brckt.middle
          case (Some(_), o, None) => o -> brckt.last
          case (None, o, None) => o -> brckt.single


  extension [F[_], O](stream: Stream[F, O | Null])
    def collectNonNull: Stream[F, O] =
      stream.collect:
        case o if o != null => o.asInstanceOf[O]

    def takeWhileNotNull: Stream[F, O] =
      stream.collectWhile:
        case o if o != null => o.asInstanceOf[O]


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
     * — <i>Description taken from Monix Observable</i>
     */
    def echoRepeated(delay: FiniteDuration): Stream[IO, A] =
      for
        queue <- Stream.eval(Queue.bounded[IO, Option[Either[Throwable, Chunk[A]]]](capacity = 0))
        _ <- Stream.supervise:
          stream.chunks
            .filter(_.nonEmpty) // The processing below requires non-empty chunks
            .attempt
            .enqueueNoneTerminated(queue)
            .compile.drain
        firstOrEnd <- Stream.eval(queue.take)
        chunk <- firstOrEnd match
          case None /*end*/ => Stream.empty
          case Some(Left(error)) => Stream.raiseError[IO](error)
          case Some(Right(firstChunk)) =>
            for
              // We assured above that firstChunk.isNonEmpty
              lastRef <- Stream.eval(Ref.of[IO, A](firstChunk.last.get))
              chunk <- Stream
                .emit(firstChunk)
                .append(Stream
                  .eval:
                    queue.take
                      .timeoutTo(delay, lastRef.get.map(last => Some(Right(Chunk(last)))))
                      .flatTap(_
                        // Update lastRef:
                        // combine Option and Either, then return the last element of the Chunk
                        .flatMap(_.toOption)
                        .flatMap(_.last)
                        .fold(IO.none)(lastRef.set))
                  .repeat
                  .unNoneTerminate
                  .rethrow)
            yield chunk
        a <- Stream.chunk(chunk)
      yield a

    //def evalStream[F[_]](s: F[Stream[F, A]])(using F: Sync[F]) =
    //  Stream.eval(s).flatten

    // TODO ---> Try fs2.keepAlive <---
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

    def mapParallelBatch[B](
      batchSizeMin: Int = DefaultBatchSizeMin,
      parallelism: Int = sys.runtime.availableProcessors,
      prefetch: Int = 0)
      (f: A => B)
      //(using file: sourcecode.FileName, line: sourcecode.Line)
    : Stream[IO, B] =
      stream
        .pipeIf(prefetch > 0):
          _.prefetchN(prefetch)
        .chunks
        .evalMap: chunk =>
          val n = chunk.size
          if n < batchSizeMin.max(2) then
            // Small chunks are not worth to parallelize
            //logger.debug(s"### mapParallelBatch ${file.value}:${line.value} prefetch=$prefetch batchSize=$batchSizeMin chunk.size=$n")
            IO(chunk.map(f))
          else
            val chunkSize = (n + parallelism - 1) / parallelism
            //logger.debug(s"### mapParallelBatch ${file.value}:${line.value} prefetch=$prefetch batchSize=$batchSizeMin chunk.size=$n chunkSize=$chunkSize concurrency=${(n + chunkSize - 1) / chunkSize}")
            chunk.iterator
              .grouped(chunkSize)
              .toVector
              .parTraverse(chunk => IO(chunk.map(f)))
              .map(_.flatten)
              .map(Chunk.from)
        .unchunks


  //extension [A](underlying: IO[Stream[IO, A]])
  //  def logTiming(
  //    toCount: A => Long = simpleCount,
  //    onComplete: (FiniteDuration, Long, OutcomeIO[A, Throwable]) => Unit,
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
  //    onComplete: (FiniteDuration, Long, OutcomeIO[A, Throwable]) => Unit,
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
