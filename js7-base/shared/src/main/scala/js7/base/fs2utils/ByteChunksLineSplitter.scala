package js7.base.fs2utils

import fs2.{Chunk, Pipe, Pull, RaiseThrowable, Stream}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.metering.CallMeter
import js7.base.utils.JavaVectors.vectorIndexOf
import scala.collection.mutable.ArrayBuilder

object ByteChunksLineSplitter:

  private val LineSizeHint = 1024
  private val BigBufferThreshold = 1024 * 1024

  private val meterRechunkBytesAtSeparator = CallMeter("ourByteChunksToLines")

  /** Collect bytes until '\n' is encountered, then emit the bytes including '\n'.
    * <p>
    * A final partial line is emitted too.
    * <p>
    * The stream of bytes remains unchanged, only the Chunks are changed.
    * <p>
    * <i>Original code was FS2 3.12.2: [[fs2.text.lines]], adapted to Byte.</i>
    */
  def byteChunksToLines[F[_], ByteSeq <: AnyRef: ByteSequence]: Pipe[F, ByteSeq, ByteSeq] =
    byteChunksToLines[F, ByteSeq]()

  /** Collect bytes until '\n' is encountered, then emit the bytes including '\n'.
    * <p>
    * A final partial line is emitted too.
    * <p>
    * The stream of bytes remains unchanged, only the Chunks are changed.
    * <p>
    * <i>Original code was FS2 3.12.2: [[fs2.text.lines]], adapted to Byte.</i>
    */
  private[fs2utils] def byteChunksToLines[F[_], ByteSeq <: AnyRef: ByteSequence as ByteSeq](
    separator: Byte = '\n',
    maxLineLength: Option[(Int, RaiseThrowable[F])] = None)
  : Pipe[F, ByteSeq, ByteSeq] =
    import ByteSeq.classTag

    def fillBuffers(
      line: ArrayBuilder[Byte],
      lines: ArrayBuilder[ByteSeq],
      bytes: Array[Byte])
    : Unit =
      var i = 0
      val byteCount = bytes.length
      while i < byteCount do
        val idx = bytes.vectorIndexOf(separator, i, byteCount)
        if idx < 0 then
          line.addAll(bytes, i, byteCount)
          i = byteCount
        else
          if line.length == 0 then
            lines += ByteSeq.unsafeWrap(bytes, i, idx + 1 - i)
          else
            line.addAll(bytes, i, idx + 1)
            lines += ByteSeq.unsafeWrap(line.result())
            line.clear()
          i = idx + 1
        end if
    end fillBuffers

    def go(
      stream: Stream[F, ByteSeq],
      line: ArrayBuilder[Byte],
      first: Boolean)
    : Pull[F, ByteSeq, Unit] =
      stream.pull.uncons.flatMap:
        case None =>
          if first then
            Pull.done
          else if line.length == 0 then
            Pull.done
          else
            Pull.output1(ByteSeq.unsafeWrap(line.result()))

        case Some((chunk, stream)) =>
          val linesBuffer = ArrayBuilder.ofRef[ByteSeq]
          chunk.foreach: byteChunk =>
            fillBuffers(line, linesBuffer, byteChunk.unsafeArray)

          maxLineLength match
            case Some((max, raiseThrowable)) if line.length > max =>
              Pull.raiseError[F](
                new LineTooLongException(line.length, max)
              )(using raiseThrowable)

            case _ =>
              Pull.output(Chunk.array(linesBuffer.result())) >>
                go(stream, line, first = false)

    stream =>
      Stream.suspend:
        go(stream, ArrayBuilder.ofByte(), first = true)
          .stream
  end byteChunksToLines

  private[fs2utils] final class LineTooLongException(val length: Int, val max: Int)
    extends RuntimeException(s"Max line size is $max but $length chars have been accumulated")
