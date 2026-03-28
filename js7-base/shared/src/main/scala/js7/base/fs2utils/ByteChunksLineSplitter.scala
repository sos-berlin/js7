package js7.base.fs2utils

import fs2.{Chunk, Pipe, Pull, RaiseThrowable, Stream}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.metering.CallMeter
import js7.base.utils.JavaVectors.vectorIndexOf
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

object ByteChunksLineSplitter:

  private val LineSizeHint = 1024
  private val BigBufferThreshold = 1024 * 1024

  private val meterRechunkBytesAtSeparator = CallMeter("ourByteChunksToLines")

  def byteChunksToLineStrings[F[_], ByteSeq: ByteSequence]: Pipe[F, ByteSeq, String] =
    byteChunksToLineStrings[F, ByteSeq]()

  /** Collect bytes until '\n' is encountered, then emit the bytes including '\n'.
    *
    * A final partial line is emitted too.
    *
    * The stream of bytes remains unchanged, only the Chunks are changed.
    *
    * <i>Original code was FS2 3.12.2: [[fs2.text.lines]], adapted to Byte.</i>
    */
  def byteChunksToLines[F[_], ByteSeq: ByteSequence]: Pipe[F, ByteSeq, ByteSeq] =
    byteChunksToLines[F, ByteSeq]()

  private[fs2utils] def byteChunksToLineStrings[F[_], ByteSeq: ByteSequence as ByteSeq](
    separator: Byte = '\n',
    maxLineLength: Option[(Int, RaiseThrowable[F])] = None)
  : Pipe[F, ByteSeq, String] =
    byteChunksToLines(_).map(_.utf8String)

  /** Collect bytes until '\n' is encountered, then emit the bytes including '\n'.
    *
    * A final partial line is emitted too.
    *
    * The stream of bytes remains unchanged, only the Chunks are changed.
    *
    * For efficiency, upstream `ByteSeq`s should have an efficient `unsafeArray` method
    * (that means, the ByteSeqs should be `Array`-based).
    *
    * <i>Original code was FS2 3.12.2: [[fs2.text.lines]], adapted to Byte.</i>
    */
  private[fs2utils] def byteChunksToLines[F[_], ByteSeq: ByteSequence as ByteSeq](
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
      val end = bytes.length
      while i < end do
        val idx = bytes.vectorIndexOf(separator, i, end)
        if idx < 0 then // no separator?
          line.addAll(bytes, i, end)
          i = end
        else if line.length == 0 then
          lines += ByteSeq.unsafeWrap(bytes, i, idx + 1 - i)
          i = idx + 1
        else
          line.addAll(bytes, i, idx + 1)
          i = idx + 1
          lines += ByteSeq.unsafeWrap(line.result())
          line.clear()
    end fillBuffers

    def go(
      stream: Stream[F, ByteSeq],
      line: ArrayBuilder[Byte],
      first: Boolean)
    : Pull[F, ByteSeq, Unit] =
      stream.pull.uncons.flatMap:
        case Some((chunk, stream)) =>
          val linesBuffer: ArrayBuilder[ByteSeq] =
            ArrayBuilder.ofRef[AnyRef](using ByteSeq.classTag.asInstanceOf[ClassTag[AnyRef]])
              .asInstanceOf
          chunk.foreach: byteSeq =>
            fillBuffers(line, linesBuffer, byteSeq.unsafeArray)

          maxLineLength match
            case Some((max, raiseThrowable)) if line.length > max =>
              Pull.raiseError[F](
                new LineTooLongException(line.length, max)
              )(using raiseThrowable)

            case _ =>
              Pull.output(Chunk.array(linesBuffer.result())) >>
                go(stream, line, first = false)

        case None =>
          if first || line.length == 0 then
            Pull.done
          else
            Pull.output1(ByteSeq.unsafeWrap(line.result()))

    stream =>
      Stream.suspend:
        go(stream, ArrayBuilder.ofByte(), first = true)
          .stream
  end byteChunksToLines

  private[fs2utils] final class LineTooLongException(val length: Int, val max: Int)
    extends RuntimeException(s"Max line size is $max but $length chars have been accumulated")
