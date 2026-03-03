package js7.base.log.reader

import cats.effect.{IO, ResourceIO}
import fs2.{Chunk, Stream}
import izumi.reflect.Tag
import java.nio.file.{Files, Path}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, DateTimeParseException}
import java.time.temporal.ChronoField.NANO_OF_SECOND
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId}
import java.util.regex.Pattern
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.file.ByteSeqFileReader.*
import js7.base.log.AnsiEscapeCodes.HighlightRegex
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogFileReader.*
import js7.base.metering.CallMeter
import js7.base.system.Java17Polyfill.getChars
import js7.base.time.EpochNano
import js7.base.time.JavaTimeExtensions.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

final class LogFileReader(val/*???*/ reader: ByteSeqFileReader[Chunk[Byte]], zoneId: ZoneId):

  def streamLines(position: Long): Stream[IO, Chunk[Byte]] =
    streamPosAndLines(position)
      .map(_._2)

  def streamKeyedLines(position: Long): Stream[IO, KeyedLogLine] =
    streamPosAndLines(position).map: (pos, byteLine) =>
      KeyedLogLine(Instant.EPOCH, pos, byteLine.utf8String)

  def streamPosAndLines(position: Long): Stream[IO, (Long, Chunk[Byte])] =
    Stream.exec:
      reader.setPosition(position)
    .append:
      reader.stream.takeWhile(_.nonEmpty)
        .through:
          byteChunksToLines
        .scanChunks(position): (pos, lines) =>
          lines.mapAccumulate(pos): (pos, line) =>
            (pos + line.size) -> (pos -> line)

  def takeUntilInstant(
    instant: Option[Instant],
    fastTimestampParser: => FastTimestampParser = FastTimestampParser(zoneId))
  : fs2.Pipe[IO, (Long, Chunk[Byte]), (Long, Chunk[Byte])] =
    stream =>
      instant.fold(stream): instant =>
        val toNanos = fastTimestampParser // call-by-name
        val endEpochNano = instant.toEpochNano
        stream.takeWhile: (_, byteLine) =>
          val epochNano = parseTimestampInLogLine(byteLine)(toNanos.apply)
          epochNano < endEpochNano


object LogFileReader:
  private val logger = Logger[this.type]

  def resource(file: Path, zoneId: ZoneId, byteChunkSize: Int): ResourceIO[LogFileReader] =
    ByteSeqFileReader.resource(file, bufferSize = byteChunkSize).map: reader =>
      new LogFileReader(reader, zoneId)

  /** Number of first bytes of a log file with a timestamp which should uniquely identify it.
    *
    * The first line of each log file starts with a timestamp including the timezone offset,
    * to uniquely identify it.
    * <p>
    * See log4j2.xml header setting. Recommended format:
    * <pre>
    * %d{yyyy-MM-dd HH:mm:ss.SSSXX} ...
    * </pre>
    */
  val UniqueHeaderSize = 30

  private val HeaderLinePattern =
    val datetime = """\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}[.,]\d{3,9}(?:Z|[+-]\d{2}(:?\d{2})?)?""".r
    s"""^$HighlightRegex?($datetime) """.r.pattern

  private val LogLinePattern: Pattern =
    val datetime = """\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}[.,]\d{3,9}""".r
    val level = """(?:trace|debug|info|TRACE|DEBUG|INFO|WARN|ERROR)""".r
    val threadInBrackets = """\[[^]]+]""".r  // Very slow!
    val logger = """[\p{Alnum}._$-]+""".r
    //val message = """(?:.*)""".r
    // threadInBrackets is slow. Also, a thread name may contain a ']'.
    //Regex(s"""^$HighlightRegex?($datetime) $level +(?:$threadInBrackets +)?$logger +-""").pattern
    Regex(s"""^$HighlightRegex?($datetime) $level """).pattern

  private val HeaderDateTimeParser: DateTimeFormatter =
    val base =
      new DateTimeFormatterBuilder()
        .appendPattern("yyyy-MM-dd")

    def withDateTimeSeparator(sep: Char) =
      new DateTimeFormatterBuilder()
        .append(base.toFormatter)
        .appendLiteral(sep)
        .appendPattern("HH:mm:ss")

    // Build one formatter that accepts either 'T' or ' ' by using optional sections
    new DateTimeFormatterBuilder()
      .append(withDateTimeSeparator('T').toFormatter)
      // Optional fraction with '.' (1..9 digits)
      .optionalStart()
      .appendLiteral('.')
      .appendFraction(NANO_OF_SECOND, 1, 9, false)
      .optionalEnd()
      // Optional fraction with ',' (1..9 digits)
      .optionalStart()
      .appendLiteral(',')
      .appendFraction(NANO_OF_SECOND, 1, 9, false)
      .optionalEnd()
      // Optional offset, e.g. Z or +02:00
      .optionalStart()
      .appendOffset("+HHMM", "")
      .optionalEnd()
      .toFormatter()

  private val meterRegex = CallMeter("LogFileReader.LogFileRegEx")

  def parseTimestampInHeaderLine[A](line: CharSequence, zoneId: ZoneId): EpochNano =
    parseTimestamp(HeaderLinePattern, line):
      headerTimestampToEpochNano(_, zoneId)

  private def headerTimestampToEpochNano(string: CharSequence, zoneId: ZoneId): EpochNano =
    try
      val array = new Array[Char](string.length)
      string.getChars(0, string.length, array, 0)
      var end = array.length
      array(10) = 'T'
      array(19) = '.'

      // Delete optional colon
      if array(array.length - 3) == ':' then
        array(array.length - 3) = array(array.length - 2)
        array(array.length - 2) = array(array.length - 1)
        end -= 1

      HeaderDateTimeParser
        .parseBest(scala.runtime.ArrayCharSequence(array, 0, end), OffsetDateTime.from, LocalDateTime.from)
        .match
          case o: OffsetDateTime => o.toInstant
          case o: LocalDateTime => o.atZone(zoneId).toInstant
        .toEpochNano
    catch case e: DateTimeParseException =>
      logger.trace("💥 " + e.toStringWithCauses)
      EpochNano.Nix

  def parseTimestampInLogLine(byteLine: fs2.Chunk[Byte])(parse: CharSequence => EpochNano): EpochNano =
    val line = byteLine.asciiCharSequence // ASCII !!! is faster than .utf8String
    parseTimestampInLogLine(line)(parse)

  def parseTimestampInLogLine(line: CharSequence)(parse: CharSequence => EpochNano): EpochNano =
    parseTimestamp(LogLinePattern, line)(parse)

  private inline def parseTimestamp(pattern: Pattern, line: CharSequence)
    (inline parse: CharSequence => EpochNano)
  : EpochNano =
    matchTimestamp(pattern, line) match
      case null => EpochNano.Nix
      case ts => parse(ts)

  private def matchTimestamp(pattern: Pattern, line: CharSequence): CharSequence | Null =
    meterRegex:
      val matcher = pattern.matcher(line)
      if matcher.find() then
        val start = matcher.start(1)
        if start >= 0 then
          line.subSequence(start, matcher.end(1))
        else
          null
      else
        null

  @TestOnly
  private[reader] def matchTimestampInLogLine(line: CharSequence): CharSequence | Null =
    matchTimestamp(LogLinePattern, line)

  def growingLogFileStream[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, ByteSeq] =
    logger.debugStream:
      growingLogFileStream2[ByteSeq](file, byteChunkSize, pollDuration, fromEnd)

  private def growingLogFileStream2[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, ByteSeq] =
    Stream.resource:
      ByteSeqFileReader.resource(file,
        bufferSize = byteChunkSize,
        waitUntilExists = Some((poll = pollDuration, timeout = 3.s)))
    .flatMap: reader =>
      Stream.eval:
        reader.read(UniqueHeaderSize).flatTap: _ =>
          if fromEnd then
            reader.seekToEnd
          else
            reader.setPosition(0)
      .flatMap: header =>
        if header.length < UniqueHeaderSize then
          Stream.sleep_[IO](pollDuration) // Delay and end stream. Caller will try again.
        else
          fs2.Stream.repeatEval:
            reader.read
          .flatMap: byteSeqs =>
            if byteSeqs.nonEmpty then
              Stream.emit(byteSeqs)
            else
              Stream.sleep_[IO](pollDuration).append:
                // TODO Wait longer before reading header again
                Stream.force:
                  // When the log file chaned, its header file changed, too
                  readHeader[ByteSeq](file).map:
                    case `header` =>
                      Stream.empty // Unchanged, continue
                    case h =>
                      logger.debug(s"Log file header has changed: ${h.utf8String}")
                      Stream.emit(null) // Changed, end the stream
            end if
          .takeWhileNotNull
    .append:
      waitUntilFileExists(file, pollDuration) // TODO Limit waiting time
    .append:
      growingLogFileStream2(file, byteChunkSize, pollDuration)

  private def waitUntilFileExists(file: Path, pollDuration: FiniteDuration): Stream[IO, Nothing] =
    logger.traceCall("waitUntilFileExists", file):
      Stream.eval:
        IO.blocking:
          Files.exists(file)
      .evalTap: exists =>
        IO.unlessA(exists):
          IO.sleep(pollDuration)
      .collectFirst:
        case true =>
      .drain

  private def readHeader[ByteSeq: ByteSequence](file: Path): IO[ByteSeq] =
    logger.traceIO("readHeader"):
      ByteSeqFileReader.resource[ByteSeq](
        file,
        waitUntilExists = Some((poll = 100.ms /*TODO*/ , timeout = 3.s))
      ).use: reader =>
        reader.read(UniqueHeaderSize)
