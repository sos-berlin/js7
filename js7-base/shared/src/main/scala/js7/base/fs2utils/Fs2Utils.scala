package js7.base.fs2utils

import cats.effect.IO
import cats.effect.kernel.Sync
import cats.syntax.monoid.*
import fs2.{Chunk, Stream}
import java.io.InputStream
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

object Fs2Utils:

  type StreamIO[A] = Stream[IO, A]
  type StreamPure[A] = Stream[fs2.Pure, A]

  /** Convert to pairs of byte position and ByteSeq. */
  def toPosAndLines[F[_], ByteSeq: ByteSequence](firstPosition: Long)
  : fs2.Pipe[F, ByteSeq, (Long, ByteSeq)] =
    _.through:
      byteChunksToLines
    .scanChunks(firstPosition): (pos, lines) =>
      lines.mapAccumulate(pos): (pos, line) =>
        (pos + line.length) -> (pos -> line)

  private object End
  private val EndStream = Stream.emit(End)

  def combineByteSeqs[F[_], ByteSeq](limit: Int)(using ByteSeq: ByteSequence[ByteSeq])
  : fs2.Pipe[F, ByteSeq, ByteSeq] =
    _.append(EndStream)
      .scan((ByteSeq.newBuilder(limit), 0, Chunk.empty[ByteSeq])):
        case ((builder, 0, _), End) =>
          (builder.clear(), 0, Chunk.empty)

        case ((builder, _, _), End) =>
          val chunk = Chunk.singleton(builder.result())
          (builder.clear(), 0, chunk)

        case ((builder, builderLength, _), byteSeq: ByteSeq @unchecked) =>
          if byteSeq.isEmpty then
            (builder, builderLength, Chunk.empty)
          else
            val sum = builderLength + byteSeq.length
            if sum <= limit then
              (builder.append(byteSeq), sum, Chunk.empty)
            else if byteSeq.length > limit then
              val chunk =
                if builderLength == 0 then
                  Chunk.singleton(byteSeq)
                else
                  Chunk(builder.result(), byteSeq)
              (builder.clear(), 0, chunk)
            else
              val chunk = Chunk.singleton(builder.result())
              (builder.clear().append(byteSeq), byteSeq.length, chunk)
      .collect:
        case (builder, _, chunk) if chunk.nonEmpty => chunk
        //case x => Chunk.empty
      .unchunks

  /** Like [[Stream.unfoldChunkEval]] but returns Chunks until a weighted limit is reached.
    * <p>
    * Upstream big elements will not be split and are returned as is (bigger then limit)
    *
    * @param limit The weight limit, must be > 0
    * @param weight The weight of an element, must be >= 0
    */
  def unfoldEvalWeighted[O](
    limit: Int,
    weight: O => Int,
    hint: Sync.Type = Sync.Type.Delay)
    [F[_]: Sync as F]
    (next: => Option[O])
  : Stream[F, Chunk[O]] =
    Stream.suspend:
      assertThat(limit > 0)
      val builder = new VectorBuilder[O]
      var accWeight = 0L
      Stream.unfoldEval(()): _ =>
        F.suspend(hint):
          @tailrec def loop(): Vector[O] =
            next match
              case None => Vector.empty
              case Some(o) =>
                val oWeight = weight(o)
                assertThat(oWeight >= 0)
                val w = accWeight + oWeight
                if w < limit then
                  builder += o
                  accWeight = w
                  loop()
                else
                  if w == limit || accWeight == 0 then
                    builder += o
                    val result = builder.result()
                    builder.clear()
                    accWeight = 0
                    result
                  else
                    val result = builder.result()
                    builder.clear()
                    builder += o // Carry over
                    accWeight = oWeight
                    result
          val result = loop()
          result.nonEmpty ? (fs2.Chunk.from(result) -> ())
      .append:
        // lazily add the remaining carry
        Stream.iterable:
          builder.nonEmpty ? fs2.Chunk.from(builder.result())

  def inputStreamToStream(in: InputStream, bufferSize: Int = 8192): Stream[IO, Chunk[Byte]] =
    endlessInputStreamToStream(in, bufferSize)
      .takeWhile(_.nonEmpty)

  /** Don't stop at EOF, but keep reading.
    *
    * Must be used with `.takeWhile(_.nonEmpty)` or some polling mechanism.
    * */
  def endlessInputStreamToStream(in: InputStream, bufferSize: Int = 8192): Stream[IO, Chunk[Byte]] =
    Stream.repeatEval:
      IO.blocking:
        val buffer = new Array[Byte](bufferSize)
        in.read(buffer) match
          case -1 => fs2.Chunk.empty
          case n => fs2.Chunk.array(buffer, 0, n)
