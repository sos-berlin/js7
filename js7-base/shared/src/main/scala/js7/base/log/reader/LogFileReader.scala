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

  private val meterReadHeader = CallMeter("LogFileReader.readHeader")

  val FastPrefixPattern: Pattern =
    Pattern.compile(s"^$HighlightRegex?20..-..-.....:..:.+ - ")

  def streamGrowingLogFile[ByteSeq: ByteSequence](
    file: Path,
    byteChunkSize: Int,
    poll: FiniteDuration,
    position: Long = 0)
    (using sourcecode.FullName, LogIndexConf)
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
    (using src: sourcecode.FullName, conf: LogIndexConf)
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
                reader.read(conf.uniqueHeaderSize)
              .flatMap: header =>
                if header.length < conf.uniqueHeaderSize then
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
                    if lastTimeHeaderRead.elapsed < (conf.checkLogFileChangePeriod max poll) then
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

  private def readHeader[ByteSeq: ByteSequence](file: Path, poll: FiniteDuration)
    (using conf: LogIndexConf)
  : IO[ByteSeq] =
    meterReadHeader:
      ByteSeqFileReader.resource[ByteSeq](file).use: reader =>
        reader.read(conf.uniqueHeaderSize)

  private val growingCounter = Atomic(0)
