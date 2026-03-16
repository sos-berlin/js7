package js7.base.fs2utils

import cats.effect.Resource.ExitCase
import cats.effect.std.AtomicCell
import cats.effect.{Concurrent, GenSpawn, IO, Ref, Sync, Temporal}
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.{Applicative, ApplicativeError, Eq, Monoid, effect}
import fs2.{Chunk, Pull, RaiseThrowable, Stream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.RichAny
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.reflect.ClassTag

object StreamExtensions:

  val DefaultBatchSizeMin = 256
  private val CedePeriod = 10.ms
  private object Beat

  type PureStream[A] = Stream[fs2.Pure, A]

  extension [A](chunk: Chunk[A])
    def :+[B](b: B): Chunk[A | B] =
      chunk ++ Chunk.singleton(b)

    def grouped(size: Int): Iterator[Chunk[A]] =
      if size <= 0 then
        throw new IllegalArgumentException(s"Chunk.grouped size must be positive: $size")
      val myGroupSize = size // size is a name in Iterator superclass, too
      var remainder = chunk

      new Iterator[Chunk[A]]:
        def hasNext = remainder.nonEmpty

        def next() =
          if remainder.isEmpty then
            throw new NoSuchElementException("Chunk.grouped next but !hasNext")
          val (a, b) = remainder.splitAt(myGroupSize)
          remainder = b
          a


  extension (chunk: Chunk[Byte])
    def byteAt(i: Int): Byte =
      // Because Chunk is not specialized for Byte, it's faster when we look up ourself.
      // Very tiny optimization appropriate only for big numbers (~1 billion) of accesses.
      chunk match
        case Chunk.ArraySlice(array, off, len) =>
          if i >= 0 && i < len then
            array(off + i) // Don't boxes Int
          else
            chunk(i)

        case Chunk.ByteBuffer(buf, off, len) =>
          if i >= 0 && i < len then
            buf.get(off + i) // Don't boxes Int
          else
            chunk(i)

        case _ =>
          chunk(i) // Boxes Int

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

    def stringAsUtf8(string: String): Chunk[Byte] =
      Chunk.array[Byte](string.getBytes(UTF_8))


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

    def cedePeriodically(using F: GenSpawn[F, Throwable]): Stream[F, O] =
      Stream.suspend:
        val nanos = CedePeriod.toNanos
        var nextCede = System.nanoTime () + nanos
        stream.chunks.evalMap: chunk =>
          if nextCede <= System.nanoTime() then
            nextCede = System.nanoTime () + nanos
            F.cede.as(chunk)
          else
            F.pure(chunk)
        .unchunks

    def collectAndFlushOnSilence(duration: FiniteDuration)(using F: Temporal[F])
    : Stream[F, Chunk[O]] =
      Stream
        .eval(AtomicCell[F].of(VectorBuilder[O]()))
        .flatMap: cell =>
          stream
            .chunks
            .evalTap: chunk =>
              cell.getAndUpdate(_ ++= chunk.asSeq)
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

    def mapAndRechunkToBytesBuffered[ByteSeq: ByteSequence](chunkSize: Int)(f: O => ByteSeq)
    : fs2.Stream[F, fs2.Chunk[ByteSeq]] =
      stream.mapChunkWeighted(f, chunkSize)(_.length)

    /** Like `chunkN` but returns Chunks until a weighted limit is reached.
      * <p>
      *   The downstream my have elements with a weight > limit, because
      *   no element is being split (no split function is provided).
      * @see [[Fs2Utils.unfoldEvalWeighted]]
      * @see [[combineWeightedInChunk]]
      */
    def chunkWeighted(limit: Int)(weight: O => Int): Stream[F, Chunk[O]] =
      mapChunkWeighted[O](identity, limit)(weight)

    def mapChunkWeighted[O2](f: O => O2, limit: Int)(weight: O2 => Int): Stream[F, Chunk[O2]] =
      stream.pull.uncons.flatMap:
        case Some((chunk, next)) =>
          def go(s: Stream[F, O], acc: VectorBuilder[O2], accWeight: Int): Pull[F, Chunk[O2], Unit] =
            s.pull.uncons1.flatMap:
              case Some((o, next)) =>
                val o2 = f(o)
                val w = weight(o2)
                if accWeight + w <= limit || acc.isEmpty then
                  acc += o2
                  go(next, acc, accWeight + w)
                else
                  val chunk = acc.result()
                  acc.clear()
                  acc += o2
                  Pull.output1(Chunk.from(chunk)) >> go(next, acc, w)
              case None =>
                Pull.output1(Chunk.from(acc.result()))

          go(Stream.chunk(chunk) ++ next, new VectorBuilder[O2], 0)
        case None =>
          Pull.done
      .stream

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
      // TODO Do not tick when idling, with small timeout the cpu is more used
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

    /** Combine elements in each Chunk until a weighted limit is reached.
      *
      * Upstream chunks will not be split.
      *
      * @see [[chunkWeighted]]
      */
    def combineWeightedInChunk(limit: Int)(weight: O => Int)(using O: Monoid[O]): Stream[F, O] =
      stream.mapChunks: chunk =>
        val chunks = new VectorBuilder[O]
        val nextO = new VectorBuilder[O]
        var size = 0L
        val it = chunk.iterator
        @tailrec def loop(): Unit =
          if it.hasNext then
            val o = it.next()
            val w = weight(o)
            require(w >= 0, s"combineWeightedInChunk($limit): weight(o) must be non-negative, but was $w")
            if size + w <= limit || size == 0 then
              size += w
              nextO += o
            else
              chunks += nextO.result().combineAll
              nextO.clear()
              nextO += o
              size = w
            loop()
        loop()
        if nextO.nonEmpty then
          chunks += nextO.result().combineAll
        Chunk.from(chunks.result())

    ///** Combine elements in each Chunk until a weighted limit is reached.
    //  *
    //  * Upstream chunks will not be split.*/
    //def mapAndCombineWeightedInChunk(limit: Int, weight: O => Int)(f: O => O2)(using O: Monoid[O]): Stream[F, O] =
    //  stream.mapChunks: chunk =>
    //    val chunks = new VectorBuilder[O]
    //    val nextO = new VectorBuilder[O]
    //    var size = 0L
    //    val it = chunk.iterator
    //    @tailrec def loop(): Unit =
    //      if it.hasNext then
    //        val o = it.next()
    //        val w = weight(o)
    //        require(w >= 0, s"combineWeightedInChunk($limit): weight(o) must be non-negative, but was $w")
    //        if size + w <= limit || size == 0 then
    //          size += w
    //          nextO += o
    //        else
    //          chunks += nextO.result().combineAll
    //          nextO.clear()
    //          nextO += o
    //          size = w
    //        loop()
    //    loop()
    //    if nextO.nonEmpty then
    //      chunks += nextO.result().combineAll
    //    Chunk.from(chunks.result())

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

    /** Take all first elements until predicate is true, including the element that not matches.
      *
      * Then end then Stream.
      * Like [[takeThrough]], but with a reverse predicated.
      */
    def takeUntil(predicate: O => Boolean): Stream[F, O] =
      stream.takeThrough(o => !predicate(o))

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


  extension[F[_], ByteSeq: ByteSequence as ByteSeq](stream: Stream[F, Chunk[ByteSeq]])
    /** Like chunkN but combines only Chunks in each Stream Chunk, without waiting for a next Chunk.
      *
      * Use these for streams with sporadic chunks.  */
    def rechunkBytes(limit: Int): Stream[F, Chunk[Byte]] =
      val bytes: Stream[F, Byte] =
        stream.mapChunks(_.combineAll)
          .combineWeightedInChunk(limit)(_.length)
          .map(_.toChunk)
          .unchunks
      bytes.chunkLimit(limit)

  extension[F[_], ByteSeq: ByteSequence](stream: Stream[F, Chunk[ByteSeq]])
    def chunkLimitBytes(limit: Int): Stream[F, Chunk[Byte]] =
      val bytes: Stream[F, Byte] =
        stream.mapChunks(_.flatMap(_.flatMap(_.toChunk)))
      bytes.chunkLimit(limit)


  extension[A](stream: Stream[IO, A])

    def mapParallelBatch[B](
      batchSizeMin: Int = DefaultBatchSizeMin,
      parallelism: Int = sys.runtime.availableProcessors,
      prefetch: Int = 0)
      (f: A => B)
      //(using loc: ScalaSourceLocation)
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
