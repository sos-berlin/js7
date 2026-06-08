package js7.base.log.reader

import cats.effect.IO
import fs2.Chunk
import java.time.{Instant, ZoneId}
import java.util.regex.{Matcher, Pattern}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.reader.LogFileReader.parseTimestampInLogLine
import js7.base.log.{AnsiEscapeCodes, reader}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.utils.ScalaUtils.syntax.*
import scala.math.Ordered.orderingToOrdered

object LogFileUtils:

  private type LogLine = KeyedByteLogLine | PosAndLine | Chunk[Byte]

  def applyLogSelection[A <: LogLine](logSelection: LogSelection)(using ZoneId)
  : fs2.Pipe[IO, A, A] =
    applyLogSelection(logSelection, FastTimestampParser())

  def applyLogSelection[A <: LogLine](
    logSelection: LogSelection,
    fastTimestampParser: => FastTimestampParser)
  : fs2.Pipe[IO, A, A] =
    _.through:
      takeUntilInstant(logSelection.end, fastTimestampParser)
    .through: stream =>
      logSelection.pattern match
        case None => stream.prefetch
        case Some(pattern) => stream.through(filterPattern(pattern))
    .pipeMaybe(logSelection.lineLimit): (stream, n) =>
      stream.take(n)

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

  def takeUntilInstant[A <: LogLine](
    instant: Option[Instant],
    fastTimestampParser: => FastTimestampParser)
  : fs2.Pipe[IO, A, A] =
    stream =>
      instant.fold(stream): instant =>
        val timestampParser = fastTimestampParser // call-by-name
        val endEpochNano = instant.toEpochNano
        stream.takeWhile: element =>
          val byteLine = element match
            case o: KeyedByteLogLine => o.byteLine
            case o: PosAndLine => o.byteLine
            case o: Chunk[Byte @unchecked] => o
          val epochNano = parseTimestampInLogLine(byteLine, timestampParser)
          epochNano < endEpochNano
