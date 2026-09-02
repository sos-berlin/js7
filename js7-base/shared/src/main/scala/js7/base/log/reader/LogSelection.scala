package js7.base.log.reader


import cats.effect.IO
import fs2.Chunk
import java.time.{Instant, ZoneId}
import java.util.regex.{Matcher, Pattern}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.reader.LogSelection.*
import js7.base.log.{AnsiEscapeCodes, reader}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.flatten
import js7.base.utils.ScalaUtils.syntax.*
import scala.math.Ordered.orderingToOrdered
import scala.math.abs

final case class LogSelection(
  end: Option[Instant] = None,
  lineLimit: Option[Long] = None,
  pattern: Option[Pattern] = None,
  maybeByteChunkSize: Option[Int] = None,
  growing: Boolean = false):

  def toKeyValues: Seq[(String, String)] =
    flatten(
      end.map(o => "end" -> o.toString),
      lineLimit.map(o => "lineLimit" -> o.toString),
      pattern.map(o => "pattern" -> o.pattern),
      growing ? ("growing" -> "true"))

  def forReader: ForReader =
    ForReader(growing, backwards, maybeByteChunkSize)

  def backwards: Boolean =
    !growing && lineLimit.exists(_ < 0)

  def pipe[A <: LogLine](using ZoneId): fs2.Pipe[IO, A, A] =
    _.through:
      takeUntilInstant:
        end.map(_.toEpochNano).map: end =>
          if backwards
          then t => t >= end
          else t => t < end
    .through: stream =>
      pattern match
        case None => stream.prefetch
        case Some(pattern) => stream.through(filterPattern(pattern))
    .pipeMaybe(lineLimit): (stream, lineLimit) =>
      val n = if lineLimit == Long.MinValue then Long.MaxValue else abs(lineLimit)
      stream.take(n)


object LogSelection:
  val all: LogSelection =
    new LogSelection()

  def apply(): LogSelection =
    all

  def lineLimit(n: Long): LogSelection =
    all.copy(lineLimit = Some(n))

  final case class ForReader(
    growing: Boolean = false,
    backwards: Boolean = false,
    maybeByteChunkSize: Option[Int] = None):

    assertThat(!growing || !backwards)

    def byteChunkSize(using conf: LogIndexConf): Int =
      maybeByteChunkSize getOrElse conf.fileBufferSize


  private type LogLine = KeyedByteLogLine | PosAndLine | Chunk[Byte]

  private def takeUntilInstant[A <: LogLine](endNotReached: Option[EpochNano => Boolean])
    (using ZoneId)
  : fs2.Pipe[IO, A, A] =
    stream =>
      endNotReached.fold(stream): endNotReached =>
        val timestampParser = FastTimestampParser()
        stream.takeWhile: logLine =>
          val byteLine = logLine match
            case o: KeyedByteLogLine => o.byteLine
            case o: PosAndLine => o.byteLine
            case o: Chunk[Byte @unchecked] => o
          val epochNano = timestampParser.parseTimestampInLogLine(byteLine)
          endNotReached(epochNano)

  private def filterPattern[A <: LogLine](pattern: Pattern): fs2.Pipe[IO, A, A] =
    stream =>
      // Requires some heap!!! heap =~ availableProcessors * logSelection.byteChunkSize
      stream.chunks.parEvalMap(sys.runtime.availableProcessors): chunk =>
        IO:
          chunk.filter: element =>
            val line = /*slow: removeHighlights*/ element match
              case o: KeyedByteLogLine => o.lineAsString
              case o: PosAndLine => o.lineAsString
              case chunk: Chunk[Byte @unchecked] => (chunk: Chunk[Byte @unchecked]).utf8String
            val matcher = pattern.matcher(line)
            tailorRegion(line, matcher)
            matcher.lookingAt() // SLOW
      .unchunks

  private def tailorRegion(line: String, matcher: Matcher): Unit =
    // It's faster if we truncate \n at end of line. And we can use $ anchor for end-of-line.
    // Also, skip ANSI highlighting at begin and end of line. It's fast.
    var b = 0
    var e = line.length
    if e >= 1 then
      if line(e - 1) == '\n' then e -= 1
      if e >= 1 && line(e - 1) == '\r' then e -= 1
      // Remove highlightíng at begin and end of line
      import AnsiEscapeCodes.resetColor
      if line.startsWith(resetColor, e - resetColor.length) then
        e -= resetColor.length
      if e >= 4 && line(0) == '\u001b' && line(1) == '[' then
        val i = line.indexOf('m', 2)
        if i > 0 then
          b = i + 1
      matcher.region(b, e max b)
    end if
