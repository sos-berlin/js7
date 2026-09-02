package js7.base.log.reader

import cats.effect.{IO, Resource}
import fs2.Chunk
import java.io.{BufferedOutputStream, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.regex.Pattern
import java.util.zip.GZIPOutputStream
import js7.base.config.Js7Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.stringAsUtf8
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.log.AnsiEscapeCodes.{bold, removeHighlights}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogFileIndexTest.*
import js7.base.log.{Logger, reader}
import js7.base.metering.CallMeter
import js7.base.problem.Checked.Ops
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
import scala.concurrent.duration.Deadline

final class LogFileIndexTest extends OurAsyncTestSuite:

  private given conf: LogIndexConf =
    LogIndexConf.fromConfig(config"""
      js7.log.index.max-bytes-per-line = $${js7.log.index.log-bytes-per-entry}  # Don't split long lines
      """.withFallback(Js7Config.defaultConfig).resolve
    ).orThrow

  "Test" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val message = "+" * (conf.logBytesPerEntry / 2)
        val lines = Vector(
          s"2026-02-12 14:00:00.000+02 Begin ... ...\n",
          s"2026-02-12 14:00:01.000+02 info [thread] class - $message 1\n",
          s"2026-02-12 14:00:02.000+02 info [thread] class - $message 2\n",
          s"2026-02-12 14:00:03.000+02 info [thread] class - $message 3\n",
          s"2026-02-12 14:00:04.000+02 info [thread] class - $message 4\n",
          s"2026-02-12 14:00:05.000+02 info [thread] class - $message 5\n",
          s"2026-02-12 14:00:06.000+02 info [thread] class - $message 6\n")
        file := lines.mkString

        LogFileIndex.fromFile(file).flatMap: logFileIndex =>
          def readOne(begin: Instant): IO[Option[String]] =
            logFileIndex.streamByteLines(begin, LogSelection())
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
          logFileIndex.streamByteLines(
              Instant.parse("2026-02-12T14:00:00+02:00"),
              LogSelection(pattern = Some(Pattern.compile("20.* - ORANGE$"))))
            .map(_.utf8String)
            .compile.toList.map: readLines =>
              assert(readLines == Vector(lines(0), lines(2)))

  "Growing" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      IO.defer:
        val message = "+" * (conf.logBytesPerEntry / 2)
        val firstLine = s"2026-02-12 14:00:01.000+02 info [thread] class - $message\n"
        file := "2026-02-12 14:00:00.000+02 Begin ... ...\n" + firstLine

        LogFileIndex.buildGrowing(file, poll = 100.ms).use: logFileIndex =>
          def readOne(begin: Instant): IO[Option[String]] =
            logFileIndex.streamByteLines(begin, LogSelection())
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

  "Backwards" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    val logFileSize = 5 * conf.logBytesPerEntry
    val lineLength = 200
    val lineCount = logFileSize / lineLength
    temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
      val startTime = "2026-08-28T00:00:00.000+03"
      writeFile(file, lineLength = lineLength, lineCount = lineCount, startTime = startTime) *>
        LogFileIndex.fromFile(file).flatMap: logFileIndex =>
          logFileIndex.streamLines(parseInstant(startTime))
            .compile.toVector.flatMap: allLines =>
              assert(allLines.length == lineCount)
              logFileIndex.streamLines(allLines(7).position, LogSelection.lineLimit(-3))
                .compile.toVector.map: reverseLines =>
                  assert(reverseLines == allLines.slice(7 - 3, 7).reverse)
                .productR:
                  // Read a part backwards
                  val n = lineCount / 2
                  logFileIndex.streamLines(
                      begin = Long.MaxValue,
                      LogSelection.lineLimit(-n))
                    .compile.toVector.map: reverseLines =>
                      assert(reverseLines.length == n)
                      assert(reverseLines == allLines.reverse.take(n))
                .productR:
                  // Read all backwards
                  logFileIndex.streamLines(
                      begin = Long.MaxValue,
                      LogSelection.lineLimit(-Long.MaxValue))
                    .compile.toVector.map: reverseLines =>
                      assert(reverseLines.length == allLines.length + 1)
                      assert(reverseLines == allLines.reverse :+
                        PosAndLine(0, Chunk.stringAsUtf8("2026-08-28T00:00:00.000+03 Begin ...\n")))

  "Test with our test.log or build.log" in :
    given ZoneId = ZoneId.systemDefault
    val begin = Timestamp.now - 1.ms
    logger.info(s"Started $begin")
    sleep(2.ms)
    logger.info("Done")

    val logFile = Path.of(if isIntelliJIdea then "logs/test.log" else "logs/build.log")
    LogFileIndex.fromFile(logFile).flatMap: logFileIndex =>
      IO.defer:
        logFileIndex.streamByteLines(begin = begin.toInstant)
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
      // Same speed as for "Latin 1" expected
      testBigFile("こんにちは") // Code points below U+10000

    "Latin 1" in:
      testBigFile("Hallå!") // Code points below U+0100, String converts faster

    def testBigFile(extra: String): IO[Assertion] =
      if !isIntelliJIdea && !sys.props.contains("test.speed") then
        IO.pure(pending)
      else
        logger.debugIO:
          val logFileSize = 1024 * 1024 * 1024
          val lineLength = 200
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

    "Read forward" in:
      testRead()

    "Read backwards" in:
      testRead(backwards = true)

    def testRead(backwards: Boolean = false): IO[Assertion] =
      if !isIntelliJIdea && !sys.props.contains("test.speed") then
        IO.pure(pending)
      else
        logger.debugIO:
          val logFileSize = 500 * 1024 * 1024
          val lineLength = 200
          val lineCount = logFileSize / lineLength
          temporaryFileResource[IO]("LogFileIndexTest-", ".tmp").use: file =>
            writeFile(file, lineLength = lineLength, lineCount = lineCount) *>
              LogFileIndex.fromFile(file).flatMap: logFileIndex =>
                (1 to 20).foldMap: _ =>
                  IO.defer:
                    val t = Deadline.now
                    logFileIndex
                      .streamLines(
                        begin = if backwards then Long.MaxValue else 0,
                        if backwards then LogSelection.lineLimit(-Long.MaxValue) else LogSelection.all)
                      .compile.count
                      .map: n =>
                        assert(n == lineCount +1/*header line*/)
                        val elapsed = t.elapsed
                        info_(s"$logFileIndex ${
                          bold(bytesPerSecondString(elapsed, lineCount * lineLength))}")
              .as(succeed)

    def info_(line: String) =
      logger.info(line)
      if !isIntelliJIdea then
        println(s"➤LogFileIndex: $line")
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
    val epochMilli = parseInstant(startTime).toEpochMilli
    Resource.fromAutoCloseable:
      IO.blocking:
        val out = new BufferedOutputStream(new FileOutputStream(file.toFile), 256 * 1024)
        new OutputStreamWriter(
          if gzip then new GZIPOutputStream(out) else out,
          UTF_8)
    .use: writer =>
      writer.write(s"$startTime Begin ...\n")
      IO.blocking:
        val t = Deadline.now
        meterWrite:
          (0 until lineCount).foreach: i =>
            val ts = dateTimeFormatter.format(Instant.ofEpochMilli(epochMilli + i))
            if isTest then assert(ts.length == 29 & 26 + lineRemainder.length == lineLength)
            writer.write(ts)
            writer.write(lineRemainder)
        logger.info("File written: " + bytesPerSecondString(t.elapsed, Files.size(file)))

  private def parseInstant(string: String): Instant =
    if Runtime.version.feature >= 25 then
      Instant.parse(string)
    else
      ZonedDateTime.parse(string).toInstant
