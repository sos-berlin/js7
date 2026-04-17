package js7.base.fs2utils

import fs2.{Chunk, Pipe, Pull, Stream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.metering.CallMeter
import js7.base.utils.JavaVectors.vectorIndexOf
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

object ByteChunksLineSplitter:

  val BreakLinesLongerThan = 32 * 1024
  val MinimumLength = 16 // Must be >= 9 or more (small 16 is for testing)
  private val LineSizeHint = 1024
  private val BigBufferThreshold = 1024 * 1024
  private val TruncationMarker = "↲".getBytes(UTF_8)
  inline private val TruncationMarkerLength = 3
  inline private val ElipsisLength = 3
  private val meterRechunkBytesAtSeparator = CallMeter("ourByteChunksToLines")

  assert(TruncationMarkerLength == TruncationMarker.length)

  def byteChunksToLineStrings[F[_], ByteSeq: ByteSequence](breakLinesLongerThan: Option[Int])
  : Pipe[F, ByteSeq, String] =
    byteChunksToLineStrings[F, ByteSeq](breakLinesLongerThan = breakLinesLongerThan: Option[Int])

  /** Collect bytes until '\n' is encountered, then emit the bytes including '\n'.
    *
    * A final partial line is emitted too.
    *
    * The stream of bytes remains unchanged, only the Chunks are changed.
    *
    * <i>Original code was FS2 3.12.2: [[fs2.text.lines]], adapted to Byte.</i>
    */
  def byteChunksToLines[F[_], ByteSeq: ByteSequence](breakLinesLongerThan: Option[Int])
  : Pipe[F, ByteSeq, ByteSeq] =
    byteChunksToLines_[F, ByteSeq](breakLinesLongerThan = breakLinesLongerThan)

  //def byteChunksToLines[F[_]: RaiseThrowable as F, ByteSeq: ByteSequence](breakLinesLongerThan: Option[Int])
  //: Pipe[F, ByteSeq, ByteSeq] =
  //  byteChunksToLines_[F, ByteSeq](breakLinesLongerThan = breakLinesLongerThan)

  private[fs2utils] def byteChunksToLineStrings[F[_], ByteSeq: ByteSequence as ByteSeq](
    separator: Byte = '\n',
    breakLinesLongerThan: Option[Int])
  : Pipe[F, ByteSeq, String] =
    stream =>
      byteChunksToLines_(separator, breakLinesLongerThan)(stream)
        .map(_.utf8String)

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
    *
    * @param breakLinesLongerThan Limit number of bytes per line.
    *                             Longer lines will be broken such that the byte count is not affected.
    */
  private def byteChunksToLines_[F[_], ByteSeq: ByteSequence as ByteSeq](
    separator: Byte = '\n',
    breakLinesLongerThan: Option[Int])
  : Pipe[F, ByteSeq, ByteSeq] =
    import ByteSeq.classTag
    val truncateAt = ((breakLinesLongerThan getOrElse Int.MaxValue max MinimumLength).toLong min Int.MaxValue).toInt

    def fillBuffers(input: Array[Byte], line: ArrayBuilder[Byte], lines: ArrayBuilder[ByteSeq])
    : Unit =
      var lineTruncated = false
      var i = 0
      val end = input.length

      def replaceTruncatedUtf(until: Int) =
        if lineTruncated then
          val u = i + ElipsisLength min until
          while i < u do
            line += '.' // Continue truncated line with dots
            i += 1
          while i < until && (input(i) & 0xc0) == 0x80 do
            line += '~' // Replace truncated multibyte UTF-8
            i += 1
          lineTruncated = false

      while i < end do
        val remaining = end - i min truncateAt - line.length
        input.vectorIndexOf(separator, i, end min i + remaining) match
          case idx if idx >= 0 =>
            replaceTruncatedUtf(idx)
            if line.length == 0 then
              lines += ByteSeq.unsafeWrap(input, i, idx + 1 - i)
              i = idx + 1
            else
              line.addAll(input, i, idx + 1 - i)
              i = idx + 1
              lines += ByteSeq.unsafeWrap(line.result())
            line.clear()

          case _ => // No separator within line length limit
            replaceTruncatedUtf(end)
            lineTruncated = true
            if line.length + end - i <= truncateAt then
              line.addAll(input, i, end)
              i = end
            else
              val remaining = end - i min truncateAt - line.length
              val until = i + remaining - 1
              var j = until - TruncationMarkerLength
              if j > i then
                if (input(j) & 0xc0) == 0x80 then j -= 1
                while j > i && (input(j - 1) & 0xc0) == 0xc0 do j -= 1 // Multibyte UTF-8
              line.addAll(input, i, j - i)
              while j < until - TruncationMarkerLength do
                line += '?'
                j += 1
              line.addAll(TruncationMarker)
              line += separator
              lines += ByteSeq.unsafeWrap(line.result())
              line.clear()
              i += remaining
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
            fillBuffers(byteSeq.unsafeArray, line, linesBuffer)
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
  end byteChunksToLines_


  private[fs2utils] final class LineTooLongException(val max: Int)
    extends RuntimeException(s"A line has more than $max bytes")
