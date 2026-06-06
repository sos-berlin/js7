package js7.base.log.reader

import cats.effect.IO
import fs2.Stream
import izumi.reflect.Tag
import java.nio.file.{Files, Path}
import java.time.ZoneId
import java.time.format.DateTimeParseException
import java.util.regex.Pattern
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.AnsiEscapeCodes.HighlightRegex
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.time.EpochNano
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

object LogFileReader:
  private val logger = Logger[this.type]

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
    val datetime =
      """\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}[.,]\d{3,6}(?:Z|[+-]\d{2}(:?\d{2})?)?""".r
    s"""^$HighlightRegex?($datetime) """.r.pattern

  private val LogLineStartPattern: Pattern =
    val level = """(?:trace|debug|info|TRACE|DEBUG|INFO|WARN|ERROR)""".r
    //val threadInBrackets = """\[[^]]+]""".r  // Very slow!
    //val logger = """[\p{Alnum}._$-]+""".r
    //val message = """(?:.*)""".r
    // threadInBrackets is slow. Also, a thread name may contain a ']'.
    //Regex(s"""^$HighlightRegex?($datetime) $level +(?:$threadInBrackets +)?$logger +-""").pattern
    Pattern.compile(s"""^$HighlightRegex?(${FastTimestampParser.DateTimeRegex}) $level """)

  val FastPrefixPattern: Pattern =
    Pattern.compile(s"^$HighlightRegex?20..-..-.....:..:.+ - ")

  private val meterRegex = CallMeter("LogFileReader.LogFileRegEx")

  def parseTimestampInHeaderLine[ByteSeq: ByteSequence](line: ByteSeq)(using ZoneId): EpochNano =
    parseTimestamp(HeaderLinePattern, line):
      headerTimestampToEpochNano

  private def headerTimestampToEpochNano[ByteSeq: ByteSequence](timestamp: ByteSeq)
    (using zoneId: ZoneId)
  : EpochNano =
    try
      val array = timestamp.toArray.map(_.toChar)
      array(10) = 'T'
      array(19) = '.'
      FastTimestampParser.parseTimestampAsNanos:
        scala.runtime.ArrayCharSequence(array, 0, array.length)
    catch case e: DateTimeParseException =>
      logger.trace("💥 " + e.toStringWithCauses)
      EpochNano.Nix

  def parseTimestampInLogLine[ByteSeq: ByteSequence](byteLine: ByteSeq)(parse: ByteSeq => EpochNano)
  : EpochNano =
    parseTimestamp(LogLineStartPattern, byteLine)(parse)

  private inline def parseTimestamp[ByteSeq: ByteSequence](pattern: Pattern, line: ByteSeq)
    (inline parse: ByteSeq => EpochNano)
  : EpochNano =
    matchTimestamp(pattern, line) match
      case null => EpochNano.Nix
      case ts => parse(ts)

  private def matchTimestamp[ByteSeq: ByteSequence](pattern: Pattern, line: ByteSeq)
  : ByteSeq | Null =
    meterRegex:
      val matcher = pattern.matcher(line.asciiCharSequence)
      if matcher.lookingAt() then
        val start = matcher.start(1)
        if start >= 0 then
          line.slice(start, matcher.end(1))
        else
          null
      else
        null

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


  @TestOnly
  private[reader] object testing:
    @TestOnly
    def parseTimestampInHeaderLine(line: String)(using ZoneId): EpochNano =
      LogFileReader.parseTimestampInHeaderLine(ByteArray(line))

    @TestOnly
    def matchTimestampInLogLine(line: String): CharSequence | Null =
      LogFileReader.matchTimestamp(LogLineStartPattern, ByteArray(line)) match
        case null => null
        case o => o.utf8String
