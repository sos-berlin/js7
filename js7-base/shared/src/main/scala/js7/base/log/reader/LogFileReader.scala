package js7.base.log.reader

import cats.effect.IO
import cats.syntax.flatMap.*
import fs2.Stream
import java.io.FileNotFoundException
import java.nio.file.Path
import java.util.regex.Pattern
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.AnsiEscapeCodes.HighlightRegex
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import scala.concurrent.duration.{Deadline, FiniteDuration}

object LogFileReader:
  private val logger = Logger[this.type]
  /** Duration until a change of current log file is detected.
    *
    * Don't check too often, don't let the user wait too long.
    * Most installation will change the log file once a day or when the file has grown big.
    * The user may wait then a short time.
    */
  private val CheckLogFileChangePeriod = 3.s

  /** Number of first bytes of a log file with a timestamp which should uniquely identify it.
    *
    * The first line of each log file starts with a timestamp including the timezone offset,
    * to uniquely identify it.
    * <p>
    * See log4j2.xml header setting. Some recommended formats:
    * <pre>
    * %d{yyyy-MM-dd HH:mm:ss.SSSX} ...
    * %d{yyyy-MM-dd'T'HH:mm:ss,SSSSSSX} ...
    * </pre>
    */
  private val longestTimestamp = "yyyy-MM-dd HH:mm:ss.SSSSSSSSS+12:34:56"
  private[reader] val UniqueHeaderSize = longestTimestamp.length + 1
  private val meterReadHeader = CallMeter("LogFileReader.readHeader")

  val FastPrefixPattern: Pattern =
    Pattern.compile(s"^$HighlightRegex?20..-..-.....:..:.+ - ")

  def streamGrowingLogFile[ByteSeq: ByteSequence](
    file: Path,
    byteChunkSize: Int,
    poll: FiniteDuration,
    position: Long = 0)
    (using sourcecode.FullName)
  : Stream[IO, ByteSeq] =
    Stream.resource:
      ByteSeqFileReader.resource(file,
        bufferSize = byteChunkSize,
        waitUntilExists = Some((poll = poll, timeout = 3.s)))
    .evalTap: reader =>
      reader.setPosition(position)
    .flatMap: reader =>
      streamGrowingLogFile(reader, file, byteChunkSize, poll)

  def streamGrowingLogFile[ByteSeq: ByteSequence](
    reader: ByteSeqFileReader[ByteSeq],
    file: Path,
    byteChunkSize: Int,
    poll: FiniteDuration)
    (using src: sourcecode.FullName)
  : Stream[IO, ByteSeq] =
    Stream.suspend:
      logger.debugStream(
        s"streamGrowingLogFile #${growingCounter.incrementAndGet()} ↖${src.value}↖ ", file
      ):
        Stream.eval:
          IO.defer:
            val pos = reader.position
            ().tailRecM: _ =>
              reader.setPosition(0).productR:
                reader.read(UniqueHeaderSize)
              .flatMap: header =>
                if header.length < UniqueHeaderSize then
                  logger.debug(s"Log file header too short: ${header.show}")
                  IO.sleep(poll).as(Left(())) // repeat
                else
                  IO.right(header)
            .productL:
              reader.setPosition(pos)
        .flatMap: header =>
          var lastTimeHeaderRead = Deadline.now
          fs2.Stream.repeatEval:
            reader.read
          .flatMap: byteSeq =>
            if byteSeq.nonEmpty then
              Stream.emit(byteSeq)
            else
              Stream.force:
                IO.sleep(poll).productR:
                  IO.defer:
                    if lastTimeHeaderRead.elapsed < (CheckLogFileChangePeriod max poll) then
                      IO.pure(Stream.empty) // Continue
                    else
                      // When the log file changed, its header file changed, too
                      lastTimeHeaderRead = Deadline.now
                      readHeader[ByteSeq](file, poll).map:
                        case `header` =>
                          Stream.empty // Unchanged, continue
                        case h =>
                          logger.debug(s"Log file header has changed: »${h.utf8String}«…")
                          // Read the our file until its end, then end our stream
                          reader.streamUntilEnd ++ Stream.emit(null) // End the Stream
                      .recover:
                        case _: FileNotFoundException =>
                          // Read our log file until its end, then end our stream
                          reader.streamUntilEnd ++ Stream.emit(null) // End the Stream
        .takeWhileNotNull

  private def readHeader[ByteSeq: ByteSequence](file: Path, poll: FiniteDuration): IO[ByteSeq] =
    meterReadHeader:
      ByteSeqFileReader.resource[ByteSeq](file).use: reader =>
        reader.read(UniqueHeaderSize)

  private val growingCounter = Atomic(0)
