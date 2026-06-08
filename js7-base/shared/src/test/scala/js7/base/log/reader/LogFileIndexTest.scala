package js7.base.log.reader

import cats.effect.{IO, Resource}
import java.io.{BufferedOutputStream, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.regex.Pattern
import java.util.zip.GZIPOutputStream
import js7.base.config.{Js7Conf, Js7Config}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.log.AnsiEscapeCodes.{bold, removeHighlights}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogFileIndexTest.*
import js7.base.metering.CallMeter
import js7.base.test.OurAsyncTestSuite
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.{isIntelliJIdea, isTest}
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.Assertion
import org.scalatest.Assertions.*
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class LogFileIndexTest extends OurAsyncTestSuite:

  override protected def testTimeout: FiniteDuration = 1.h

  override def resourceForIORuntime =
    super.resourceForIORuntime.flatMap: _ =>
      Js7Conf.registerInEnvironment(Js7Config.defaultConfig)

  "Test" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val message = "+" * (LogFileIndex.LogBytesPerEntry / 2)
        val lines = Vector(
          s"2026-02-12 14:00:00.000+02 HEADER ...\n",
          s"2026-02-12 14:00:01.000+02 info [thread] class - $message 1\n",
          s"2026-02-12 14:00:02.000+02 info [thread] class - $message 2\n",
          s"2026-02-12 14:00:03.000+02 info [thread] class - $message 3\n",
          s"2026-02-12 14:00:04.000+02 info [thread] class - $message 4\n",
          s"2026-02-12 14:00:05.000+02 info [thread] class - $message 5\n",
          s"2026-02-12 14:00:06.000+02 info [thread] class - $message 6\n")
        file := lines.mkString

        LogFileIndex.fromFile(file).flatMap: logFileIndex =>
          def readOne(begin: Instant): IO[Option[String]] =
            logFileIndex.streamLines(begin, LogSelection())
              .map(_.utf8String)
              .head.compile.last

          for
            _ <- readOne(Instant.parse("2026-02-12T14:00:01+02:00")).map: line =>
              assert(line contains lines(1))
            _ <- readOne(Instant.parse("2026-02-12T14:00:05.000000001+02:00")).map: line =>
              assert(line contains lines(6))
            _ <- readOne(Instant.parse("2026-02-12T14:00:05+02:00")).map: line =>
              assert(line contains lines(5))
            _ <- readOne(Instant.parse("2026-02-12T14:00:04.999999999+02:00")).map: line =>
              assert(line contains lines(5))
            _ <- readOne(Instant.parse("2026-02-12T14:00:00+02:00")).map: line =>
              assert(line contains lines(1))
            _ <- readOne(Instant.parse("2000-01-01T14:00:00+02:00")).map: line =>
              assert(line contains lines(1))
            _ <- readOne(Instant.parse("2222-01-01T14:00:00+02:00")).map: line =>
              assert(line.isEmpty)
          yield succeed

  "pattern" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val lines = Vector(
          bold("2026-02-12 14:00:00.000+02 info [thread] class - ORANGE") + "\n",
          "2026-02-12 14:00:01.000+02 info [thread] class - CITRON\n",
          "2026-02-12 14:00:02.000+02 info [thread] class - ORANGE\n")
        file := lines.mkString
        LogFileIndex.fromFile(file).flatMap: logFileIndex =>
          logFileIndex.streamLines(
              Instant.parse("2026-02-12T14:00:00+02:00"),
              LogSelection(pattern = Some(Pattern.compile("20.* - ORANGE$"))))
            .map(_.utf8String)
            .compile.toList
            .map: readLines =>
              assert(readLines == Vector(lines(0), lines(2)))

  "Growing" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val message = "+" * (LogFileIndex.LogBytesPerEntry / 2)
        val firstLine = s"2026-02-12 14:00:01.000+02 info [thread] class - $message\n"
        file := "2026-02-12 14:00:00.000+02 HEADER ...\n" + firstLine

        LogFileIndex.buildGrowing(file, poll = 100.ms).use: logFileIndex =>
          def readOne(begin: Instant): IO[Option[String]] =
            logFileIndex.streamLines(begin, LogSelection())
              .map(_.utf8String)
              .head.compile.last

          for
            _ <- readOne(Instant.parse("2026-02-12T14:00:01+02:00")).map: line =>
              assert(line contains firstLine)
            anotherLine = s"2026-02-12 15:00:00.000+02 info [thread] class - $message\n"
            _ =
              file ++= anotherLine
              awaitAndAssert:
                logFileIndex.lastEpochNano == Instant.parse("2026-02-12T15:00:00+02:00").toEpochNano
            _ <- readOne(Instant.parse("2026-02-12T15:00:00+02:00")).map: line =>
              assert(line contains anotherLine)
          yield succeed

  "Test with test.log" in :
    given ZoneId = ZoneId.systemDefault
    val begin = Timestamp.now - 1.ms
    logger.info(s"Started $begin")
    sleep(2.ms)
    logger.info("Done")

    val logFile = Path.of(if isIntelliJIdea then "logs/test.log" else "logs/build.log")
    LogFileIndex.fromFile(logFile).flatMap: logFileIndex =>
      IO.defer:
        logFileIndex.streamLines(begin = begin.toInstant, LogSelection())
          .through:
            byteChunksToLines(breakLinesLongerThan = None)
          .filter: byteLine =>
            val line = removeHighlights(byteLine.utf8String)
            line.contains("LogFileIndexTest - Done") && locally:
              assert(line.startsWith(begin.toString.take(5)))
              true
          .compile.last
          .map: last =>
            assert(last.isDefined)

  "1 GiB debug-log file" - {
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    "Japanese" in:
      testBigFile("こんにちは") // Code points below U+10000

    "Latin 1" in:
      testBigFile("Hallå!") // Code points below U+0100, String converts faster

    def testBigFile(extra: String): IO[Assertion] =
      if !isIntelliJIdea && !sys.props.contains("test.speed") then
        IO.pure(pending)
      else
        def info_(line: String) =
          logger.info(line)
          if !isIntelliJIdea then
            println(s"➤LogFileIndex: $line")

        logger.debugIO:
          val logFileSize = 1024 * 1024 * 1024
          val lineLength = 130
          val lineCount = logFileSize / lineLength
          temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
            writeFile(file, lineLength = lineLength, lineCount = lineCount, extra = extra) *>
              (1 to 20).foldMap: _ =>
                IO.defer:
                  System.gc()
                  val usedMemory = sys.runtime.totalMemory - sys.runtime.freeMemory
                  def memInfo = s"total=${toKiBGiB(sys.runtime.totalMemory)} free=${
                    toKiBGiB(sys.runtime.freeMemory)}"
                  logger.debug(memInfo)
                  val t = Deadline.now
                  LogFileIndex.fromFile(file).flatMap: logFileIndex =>
                    IO:
                      val elapsed = t.elapsed
                      System.gc()
                      val used = sys.runtime.totalMemory - sys.runtime.freeMemory
                      logger.debug(memInfo)
                      info_(s"$logFileIndex ${toKiBGiB(used - usedMemory)}? ${
                        bold(bytesPerSecondString(elapsed, lineCount * lineLength))}")
                      //logger.info(s"$logFileIndex ${
                      //  bold(itemsPerSecondString(elapsed, lineCount, "lines"))}")
            .as(succeed)
  }


object LogFileIndexTest:
  private val logger = Logger[this.type]
  private val meterWrite = CallMeter("LogFileIndex.write")
  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSX")
    .withZone(ZoneId.of("Europe/Mariehamn"))

  def writeFile(
    file: Path,
    lineLength: Int,
    lineCount: Int,
    startTime: String = "2026-02-12T00:00:00.000+02",
    extra: String = "",
    gzip: Boolean = false)
    (using ZoneId)
  : IO[Unit] =
    FastTimestampParser.parseTimestampAsNanos(startTime) // Must be parseable
    val lineRemainder =
      val middle = s" info  js7-7  js7.logger - message "
      middle + extra + "." * (lineLength - startTime.length - middle.length - extra.length - 1) + "\n"
    assert(lineRemainder.length == lineLength - startTime.length)
    val epochMilli = Instant.parse(startTime).toEpochMilli
    Resource.fromAutoCloseable:
      IO.blocking:
        val out = new BufferedOutputStream(new FileOutputStream(file.toFile), 256 * 1024)
        new OutputStreamWriter(
          if gzip then new GZIPOutputStream(out) else out,
          UTF_8)
    .use: writer =>
      writer.write(s"$startTime+02:00 ...\n")
      IO.blocking:
        val t = Deadline.now
        meterWrite:
          (0 until lineCount).foreach: i =>
            val ts = dateTimeFormatter.format(Instant.ofEpochMilli(epochMilli + i))
            if isTest then assert(ts.length == 29 & 26 + lineRemainder.length == lineLength)
            writer.write(ts)
            writer.write(lineRemainder)
        logger.info("File written: " + bytesPerSecondString(t.elapsed, Files.size(file)))
