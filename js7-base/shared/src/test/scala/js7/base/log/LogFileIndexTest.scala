package js7.base.log

import cats.effect.{IO, Resource}
import java.io.{BufferedOutputStream, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.log.AnsiEscapeCodes.{bold, removeHighlights}
import js7.base.log.LogFileIndexTest.*
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.{isIntelliJIdea, isTest}
import org.scalatest.Assertion
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class LogFileIndexTest extends OurAsyncTestSuite:

  override protected def testTimeout: FiniteDuration = 1.h

  "Test" in:
    val zoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val message = "+" * (LogFileIndex.BytesPerEntry / 2)
        val lines = ArrayBuffer.empty[String]
        lines += "2026-02-12 14:00:00.000 HEADER ...\n"
        lines += s"2026-02-12 14:00:01.000 info thread class - $message 1\n"
        lines += s"2026-02-12 14:00:02.000 info thread class - $message 2\n"
        lines += s"2026-02-12 14:00:03.000 info thread class - $message 3\n"
        lines += s"2026-02-12 14:00:04.000 info thread class - $message 4\n"
        lines += s"2026-02-12 14:00:05.000 info thread class - $message 5\n"
        lines += s"2026-02-12 14:00:06.000 info thread class - $message 6\n"

        val content = lines.mkString
        file := content
        LogFileIndex.resource(file, zoneId).use: logFileIndex =>
          def read(start: Instant): IO[Option[String]] =
            logFileIndex.streamSection(start).map(_.utf8String).head.compile.last

          for
            _ <- read(Instant.parse("2026-02-12T12:00:01Z")).map: line =>
              assert(line contains lines(1))
            _ <- read(Instant.parse("2026-02-12T12:00:05.000000001Z")).map: line =>
              assert(line contains lines(6))
            _ <- read(Instant.parse("2026-02-12T12:00:05Z")).map: line =>
              assert(line contains lines(5))
            _ <- read(Instant.parse("2026-02-12T12:00:04.999999999Z")).map: line =>
              assert(line contains lines(5))
            _ <- read(Instant.parse("2026-02-12T12:00:00Z")).map: line =>
              assert(line contains lines(1))
            _ <- read(Instant.parse("2000-01-01T12:00:00Z")).map: line =>
              assert(line contains lines(1))
            _ <- read(Instant.parse("2222-01-01T12:00:00Z")).map: line =>
              assert(line.isEmpty)
          yield succeed

  "Test with test.log" in :
    sleep(2.ms)
    val start = Timestamp.now
    logger.info(s"Started $start")
    sleep(2.ms)
    logger.info("Done")

    val logFile = Path.of(if isIntelliJIdea then "logs/test.log" else "logs/build.log")
    LogFileIndex.resource(logFile).use: logFileIndex =>
      IO.defer:
        logFileIndex.streamSection(start = start.toInstant)
          .takeWhile(_.nonEmpty)
          .through:
            byteChunksToLines
          .filter: byteLine =>
            val line = removeHighlights(byteLine.utf8String)
            line.contains("LogFileIndexTest - Done") && locally:
              assert(line.startsWith(start.toString.take(5)))
              true
          .compile.last
          .map: last =>
            assert(last.isDefined)

  "128 MiB debug-log file" - {
    "Japanese" in:
      testBigFile("こんにちは") // Code points below U+10000

    "Latin 1" in:
      testBigFile("Hallå!") // Code points below U+0100, String converts faster

    def testBigFile(extra: String): IO[Assertion] =
      if !isIntelliJIdea then
        IO.pure(pending)
      else
        logger.debugIO:
          // For 1GB log file, LogFileIndex seems to require 90MB heap !!!
          val logFileSize = 1024 * 1024 * 128
          val lineLength = 130
          val lineCount = logFileSize / lineLength
          temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
            writeFile(file, lineLength = lineLength, lineCount = lineCount, extra) *>
              (1 to 10).foldMap: _ =>
                IO.defer:
                  System.gc()
                  val usedMemory = sys.runtime.totalMemory - sys.runtime.freeMemory
                  def memInfo = s"total=${toKiBGiB(sys.runtime.totalMemory)} free=${
                    toKiBGiB(sys.runtime.freeMemory)}"
                  logger.info(memInfo)
                  val t = Deadline.now
                  LogFileIndex.resource(file).use: logFileIndex =>
                    IO:
                      val elapsed = t.elapsed
                      System.gc()
                      val used = sys.runtime.totalMemory - sys.runtime.freeMemory
                      logger.debug(memInfo)
                      logger.info(s"Required memory: ${toKiBGiB(used - usedMemory)} (?)")
                      logger.info(s"$logFileIndex ${
                        bold(bytesPerSecondString(elapsed, lineCount * lineLength))}")
                      logger.info(s"$logFileIndex ${
                        bold(itemsPerSecondString(elapsed, lineCount, "lines"))}")
            .as(succeed)
  }


object LogFileIndexTest:
  private val logger = Logger[this.type]
  private val meterWrite = CallMeter("LogFileIndex.write")
  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
    .withZone(ZoneId.of("Europe/Mariehamn"))

  private def writeFile(file: Path, lineLength: Int, lineCount: Int, extra: String): IO[Unit] =
    val aTimestamp = "2026-02-12T00:00:00.000"
    val lineRemainder =
      val middle = s" info  js7-7  js7.logger - message "
      middle + extra + "." * (lineLength - aTimestamp.length - middle.length - extra.length - 1) + "\n"
    assert(lineRemainder.length == lineLength - aTimestamp.length)
    val epochMilli = Timestamp("2026-02-12T00:00:00Z").toInstant.toEpochMilli
    Resource.fromAutoCloseable:
      IO.blocking:
        new OutputStreamWriter(
          new BufferedOutputStream(new FileOutputStream(file.toFile), 256 * 1024),
          UTF_8)
    .use: writer =>
      writer.write("2026-02-12T00:00:00.000 ... timezone=Europe/Mariehamn ...\n")
      IO.blocking:
        val t = Deadline.now
        meterWrite:
          (0 until lineCount).foreach: i =>
            val ts = dateTimeFormatter.format(Instant.ofEpochMilli(epochMilli + i))
            if isTest then assert(ts.length == 23 & 23 + lineRemainder.length == lineLength)
            writer.write(ts)
            writer.write(lineRemainder)
        logger.info(bold("File written: " + bytesPerSecondString(t.elapsed, Files.size(file))))
