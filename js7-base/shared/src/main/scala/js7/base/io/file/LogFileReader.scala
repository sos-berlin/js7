package js7.base.io.file

import cats.effect.IO
import fs2.{Chunk, Stream}
import izumi.reflect.Tag
import java.nio.file.{Files, Path}
import java.util.regex.Pattern
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.io.file.ByteSeqFileReader.*
import js7.base.log.AnsiEscapeCodes.HighlightRegex
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.time.ScalaTime.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

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
    val datetime = """\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}[.,]\d{3,9}(?:Z|[+-]\d\d(:?\d\d)?)?""".r
    s"""^$HighlightRegex?($datetime) """.r.pattern

  private val LogLinePattern: Pattern =
    val datetime = """\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}[.,]\d{3,9}""".r
    val level = """(?:trace|debug|info|TRACE|DEBUG|INFO|WARN|ERROR)""".r
    val thread = """[\p{Alnum}._$-]+""".r
    val logger = """[\p{Alnum}._$-]+""".r
    //val message = """(?:.*)""".r
    Regex(s"""^$HighlightRegex?($datetime) $level +(?:$thread +)?$logger +-""").pattern

  private val meterRegex = CallMeter("LogFileReader.LogFileRegEx")

  def parseTimestampInLogLine[@specialized(Long) A](line: CharSequence, error: A)(parse: CharSequence => A): A =
    parseTimestamp(LogLinePattern, line, error)(parse)

  /** Returns timestamp section iff `line` contains a valid log line. */
  @TestOnly
  private[file] def matchTimestampInLogLine(line: CharSequence): CharSequence | Null =
    matchTimestamp(LogLinePattern, line)

  def parseTimestampInHeaderLine[A](line: CharSequence, error: A)(parse: CharSequence => A): A =
    parseTimestamp(HeaderLinePattern, line, error)(parse)

  private def parseTimestamp[@specialized(Long) A](pattern: Pattern, line: CharSequence, error: A)
    (parse: CharSequence => A)
  : A =
    matchTimestamp(pattern, line) match
      case null => error
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

  //private inline def matchTimestampRaw(pattern: Pattern, line: CharSequence): (Int, Int) | Null =
  //  meterRegex:
  //    val matcher = pattern.matcher(line)
  //    if matcher.find() then
  //      val start = matcher.start(1)
  //      if start >= 0 then
  //        start -> matcher.end(1)
  //      else
  //        null
  //    else
  //      null

  def growingLogFileStream[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, Chunk[ByteSeq]] =
    logger.debugStream:
      growingLogFileStream2[ByteSeq](file, byteChunkSize, pollDuration, fromEnd)

  private def growingLogFileStream2[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, Chunk[ByteSeq]] =
    Stream.resource:
      resource(file, waitUntilExists = Some((poll = pollDuration, timeout = 3.s)))
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
          reader.chunkStreamX(byteChunkSize).flatMap: byteSeqs =>
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
      resource[ByteSeq](file, waitUntilExists = Some((poll = 100.ms /*TODO*/ , timeout = 3.s))).use: reader =>
        reader.read(UniqueHeaderSize)
