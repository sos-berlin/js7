package js7.base.log.reader

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.zip.GZIPOutputStream
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.config.{Js7Conf, Js7Config}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.{temporaryDirectoryResource, temporaryFileResource}
import js7.base.io.file.watch.BasicDirectoryWatch
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.LogLevel.{Debug, Error, Info}
import js7.base.log.Logger
import js7.base.log.reader.LogDirectoryIndexTest.*
import js7.base.log.reader.recompressors.LogFileIndexConf
import js7.base.problem.Problems.{IncompleteLogFileProblem, InvalidTimestampInLogFileProblem}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTime.extensions.+
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.AutoClosing.syntax.use
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.isIntelliJIdea
import js7.tester.ScalaTestUtils
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.concurrent.duration.Deadline

// For more tests, see JLogDirectoryIndexTester, LogFileTest and LogFileClusterTest.
final class LogDirectoryIndexTest extends OurAsyncTestSuite:

  override def resourceForIORuntime =
    super.resourceForIORuntime.flatMap: _ =>
      Js7Conf.registerInEnvironment(Js7Config.defaultConfig)

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  "LogDirectoryIndex provides a LogIndex for each logFilePrefix and LogLevel" in:
    temporaryDirectoryResource[IO]("LogDirectoryIndex-").use: dir =>
      dir / "A.log" := "2026-08-14T12:00:00,000+03 info  js7.test.Test - ...\n"
      autoClosing(GZIPOutputStream(FileOutputStream((dir / "A-2026-08-14-1.log.gz").toFile))):
        _.write("2026-08-14T10:00:00,000+03 info  js7.test.Test - ...\n".getBytes(UTF_8))
      dir / "A-error.log" := "2026-08-14T12:00:00,000+03 info  js7.test.Test - ...\n"
      dir / "A-debug.log" := "2026-08-14T12:00:00,000+03 debug  js7.test.Test - ...\n"
      dir / "B.log" := "2026-08-14T12:00:00,000+03 info  js7.test.Test - ...\n"
      dir / "X.log" := "2026-08-14T13:00:00,000+03 info  js7.test.Test - ...\n"

      given Config = ConfigFactory.empty
      LogDirectoryIndex.resource(dir, Set("A", "B")).use: logDirectoryIndex =>
        logDirectoryIndex.logIndex("A", Info).map: logIndex =>
          assert(logIndex.files.map(_.getFileName.toString).toSet ==
            Set("A.log", "A-2026-08-14-1.log.gz"))
        .productR:
          logDirectoryIndex.logIndex("A", Error).map: logIndex =>
            assert(logIndex.files.map(_.getFileName.toString) == Seq("A-error.log"))
        .productR:
          logDirectoryIndex.logIndex("A", Debug).map: logIndex =>
            assert(logIndex.files.map(_.getFileName.toString) == Seq("A-debug.log"))
        .productR:
          logDirectoryIndex.logIndex("B", Info).map: logIndex =>
            assert(logIndex.files.map(_.getFileName.toString) == Seq("B.log"))
        .productR:
          logDirectoryIndex.logIndex("X", Info).attempt.map:
            case Left(t: NoSuchElementException) => succeed // logFilePrefix X is not watched
            case x => fail(s"Unexpected: $x")
        .productR:
          IO.defer:
            // Archive a log file according to the usual log4j2 protocol
            autoClosing(GZIPOutputStream(FileOutputStream((dir / "A-2026-08-14-1.log.gz").toFile))):
              _.write("2026-08-14T12:00:00,000+03 ERROR  js7.test.Test - ...\n".getBytes(UTF_8))
            dir / "A.log" := "2026-08-14T13:00:00,000+03 info  js7.test.Test - ...\n"
            logDirectoryIndex.logIndex("A", Info).map: logIndex =>
              awaitAndAssert(logIndex.files.map(_.getFileName.toString).toSet == Set(
                "A.log",
                "A-2026-08-14-1.log.gz"))

  "LogFile" - {
    "File is (still) to short" in:
      temporaryFileResource[IO]("LogDirectoryIndexTest-", ".log").use: file =>
        file :=
          """2026-06-25T00:00:00,111 Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.test.Test - ...""".stripMargin.getBytes(UTF_8)
        for
          checked <- LogFile.read(file)
        yield
          assert(checked == Left(IncompleteLogFileProblem(file)))

    "Missing header line" in:
      temporaryFileResource[IO]("LogDirectoryIndexTest-", ".log").use: file =>
        file :=
          """2026-06-25T00:00:00,111+03 info  js7.test.Test - MISSING HEADER LINE
            |2026-06-25T00:00:00,999+03 info  js7.test.Test - ...
            |""".stripMargin
        for
          logFile <- LogFile.read(file).orThrow
        yield
          assert(logFile.fileInstant == Instant.parse("2026-06-25T00:00:00.111+03:00"))

    "Invalid timestamp in header line" in:
      temporaryFileResource[IO]("LogDirectoryIndexTest-", ".log").use: file =>
        file :=
          """2026-06-25T00:00:00,111+?? Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.test.Test - ...
            |""".stripMargin
        for
          checked <- LogFile.read(file)
        yield
          assert(checked ==
            Left(InvalidTimestampInLogFileProblem(file, "2026-06-25T00:00:00,111+?? ...")))

    "Invalid timestamp in first log line" in:
      temporaryFileResource[IO]("LogDirectoryIndexTest-", ".log").use: file =>
        file :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+?? info  js7.test.Test - ...
            |""".stripMargin
        for
          checked <- LogFile.read(file)
        yield
          assert(checked ==
            Left(InvalidTimestampInLogFileProblem(file, "2026-06-25T00:00:00,999+?? ...")))

    "Proper log file" in:
      temporaryFileResource[IO]("LogDirectoryIndexTest-", ".log").use: file =>
        file :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.journal.Journal - ...
            |""".stripMargin
        for
          logFile <- LogFile.read(file).orThrow
        yield
          assert(logFile.fileInstant == Instant.parse("2026-06-25T00:00:00.999+03:00"))
  }

  "A continuous stream of log lines in all files" in:
    temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
      val startInstant = ZonedDateTime.parse("2026-03-01T00:00:00.000+02").toInstant
      IO:
        var i = 0
        (0 until 3).foreach: d =>
          val midnight = startInstant + 24.h * d
          (0 until 3).foreach: h =>
            val hour = midnight + h.h
            val gzFile = dir / s"TEST-${hour.atZone(zoneId).toLocalDate}-$h.log.gz"
            autoClosing(
              GZIPOutputStream(BufferedOutputStream(FileOutputStream(gzFile.toFile)))
            ): out =>
              out.write:
                (headerTimestampFormatter.format(hour.atZone(zoneId)) + " Begin ...\n").getBytes(UTF_8)
              (1 to 3).foreach: s =>
                i += 1
                out.write:
                  s"${timestampFormatter.format((hour + s.s).atZone(zoneId))} info LogDirectoryIndexTest - MESSAGE $i\n"
                    .getBytes(UTF_8)
      .productR:
        given LogFileIndexConf = LogFileIndexConf.forTest
        given Config = ConfigFactory.empty()
        LogDirectoryIndex.resource(dir, logFilePrefixes = Set("TEST")).use: logDirectoryIndex =>
          logDirectoryIndex.logIndex(logFilePrefix = "TEST", Info).flatMap: logIndex =>
            /// Read *all* log files as text lines ///
            logIndex.byteLineStream(startInstant, LogSelection())
              .map(_.utf8String)
              .compile.toList.map: lines =>
                assert(lines == List(
                  // Because we read from the very first log file, NO INDEXING OCCURS and
                  // we read from the start of the file, including the header line.
                  "2026-03-01 00:00:00.000+02 Begin ...\n",
                  "2026-03-01 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 1\n",
                  "2026-03-01 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 2\n",
                  "2026-03-01 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 3\n",

                  // Header lines included due to sequential reading without LogFileIndex
                  "2026-03-01 01:00:00.000+02 Begin ...\n",
                  "2026-03-01 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 4\n",
                  "2026-03-01 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 5\n",
                  "2026-03-01 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 6\n",

                  "2026-03-01 02:00:00.000+02 Begin ...\n",
                  "2026-03-01 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 7\n",
                  "2026-03-01 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 8\n",
                  "2026-03-01 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 9\n",

                  "2026-03-02 00:00:00.000+02 Begin ...\n",
                  "2026-03-02 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 10\n",
                  "2026-03-02 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 11\n",
                  "2026-03-02 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 12\n",

                  "2026-03-02 01:00:00.000+02 Begin ...\n",
                  "2026-03-02 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 13\n",
                  "2026-03-02 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 14\n",
                  "2026-03-02 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 15\n",

                  "2026-03-02 02:00:00.000+02 Begin ...\n",
                  "2026-03-02 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 16\n",
                  "2026-03-02 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 17\n",
                  "2026-03-02 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 18\n",

                  "2026-03-03 00:00:00.000+02 Begin ...\n",
                  "2026-03-03 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 19\n",
                  "2026-03-03 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 20\n",
                  "2026-03-03 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 21\n",

                  "2026-03-03 01:00:00.000+02 Begin ...\n",
                  "2026-03-03 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 22\n",
                  "2026-03-03 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 23\n",
                  "2026-03-03 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 24\n",

                  "2026-03-03 02:00:00.000+02 Begin ...\n",
                  "2026-03-03 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 25\n",
                  "2026-03-03 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 26\n",
                  "2026-03-03 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 27\n"))
              .productR:
                val instant = ZonedDateTime.parse("2026-03-01T00:00:01.000+02").toInstant
                logIndex.byteLineStream(instant, LogSelection())
                  .take(5)
                  .map(_.utf8String)
                  .compile.toList.map: lines =>
                    assert(lines == List(
                      "2026-03-01 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 1\n",
                      "2026-03-01 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 2\n",
                      "2026-03-01 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 3\n",

                      // Header lines included due to sequential reading without LogFileIndex
                      "2026-03-01 01:00:00.000+02 Begin ...\n",
                      "2026-03-01 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 4\n"))
              .productR:
                /// Read all log files as KeyedByteLogLine ///
                logIndex.keyedByteLogLineStream(startInstant, LogSelection())
                  .compile.toList
              .flatMap: keyedByteLogLines =>
                assert(keyedByteLogLines.map(_.lineAsString) == List(
                  "2026-03-01 00:00:00.000+02 Begin ...\n",
                  "2026-03-01 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 1\n",
                  "2026-03-01 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 2\n",
                  "2026-03-01 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 3\n",

                  "2026-03-01 01:00:00.000+02 Begin ...\n",
                  "2026-03-01 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 4\n",
                  "2026-03-01 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 5\n",
                  "2026-03-01 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 6\n",

                  "2026-03-01 02:00:00.000+02 Begin ...\n",
                  "2026-03-01 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 7\n",
                  "2026-03-01 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 8\n",
                  "2026-03-01 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 9\n",

                  "2026-03-02 00:00:00.000+02 Begin ...\n",
                  "2026-03-02 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 10\n",
                  "2026-03-02 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 11\n",
                  "2026-03-02 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 12\n",

                  "2026-03-02 01:00:00.000+02 Begin ...\n",
                  "2026-03-02 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 13\n",
                  "2026-03-02 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 14\n",
                  "2026-03-02 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 15\n",

                  "2026-03-02 02:00:00.000+02 Begin ...\n",
                  "2026-03-02 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 16\n",
                  "2026-03-02 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 17\n",
                  "2026-03-02 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 18\n",

                  "2026-03-03 00:00:00.000+02 Begin ...\n",
                  "2026-03-03 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 19\n",
                  "2026-03-03 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 20\n",
                  "2026-03-03 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 21\n",

                  "2026-03-03 01:00:00.000+02 Begin ...\n",
                  "2026-03-03 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 22\n",
                  "2026-03-03 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 23\n",
                  "2026-03-03 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 24\n",

                  "2026-03-03 02:00:00.000+02 Begin ...\n",
                  "2026-03-03 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 25\n",
                  "2026-03-03 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 26\n",
                  "2026-03-03 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 27\n"))

                assert(keyedByteLogLines(3).posAndLine.lineAsString ==
                  "2026-03-01 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 3\n")
                logIndex.keyedByteLogLineStream(keyedByteLogLines(3).logLineKey, LogSelection())
                  .take(4)
                  .compile.toList
                  .map: keyedByteLogLines =>
                    assert(keyedByteLogLines.map(_.lineAsString) == List(
                      // Same line again (user may want to skip it)
                      "2026-03-01 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 3\n",
                      "2026-03-01 01:00:00.000+02 Begin ...\n",
                      "2026-03-01 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 4\n",
                      "2026-03-01 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 5\n"))
                .productR:
                  logIndex.keyedByteLogLineStream(keyedByteLogLines(12).logLineKey, LogSelection())
                    .take(4)
                    .compile.toList
                    .map: keyedByteLogLines =>
                      assert(keyedByteLogLines.map(_.lineAsString) == List(
                        "2026-03-02 00:00:00.000+02 Begin ...\n",
                        "2026-03-02 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 10\n",
                        "2026-03-02 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 11\n",
                        "2026-03-02 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 12\n"))

  "Add a .log.gz" in:
    temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
      val startInstant = ZonedDateTime.parse("2026-03-01T00:00:00.000+02").toInstant
      def instantToFile(instant: Instant) =
        dir / s"TEST-${instant.atZone(zoneId).toLocalDate}.log.gz"

      def writeFile(instant: Instant): Unit =
        val file = dir / "TEST.log"
        BufferedOutputStream(FileOutputStream(file.toFile)).use: out =>
          out.write:
            (headerTimestampFormatter.format(instant.atZone(zoneId)) + " Begin ...\n").getBytes(UTF_8)
          out.write:
            s"${timestampFormatter.format(instant.atZone(zoneId))} info LogDirectoryIndexTest - MESSAGE\n"
              .getBytes(UTF_8)

        sleep(BasicDirectoryWatch.systemWatchDelay + 200.ms) // For macOS: delay before FileDeleted
        val gzFile = instantToFile(instant)
        GZIPOutputStream(BufferedOutputStream(FileOutputStream(gzFile.toFile))).use: out =>
          Files.copy(file, out)

        // Trigger FileDeleted / LogFileDeleted,
        // telling LogIndex that the .log.gz is complete and readable now
        Files.delete(file)
      end writeFile

      val aFile = instantToFile(startInstant)
      val bFile = instantToFile(startInstant + 24.h)
      writeFile(startInstant)
      writeFile(startInstant + 24.h)

      given LogFileIndexConf = LogFileIndexConf.forTest
      given Config = ConfigFactory.empty()
      LogDirectoryIndex.resource(dir, logFilePrefixes = Set("TEST")).use: logDirectoryIndex =>
        logDirectoryIndex.logIndex(logFilePrefix = "TEST", Info).flatMap: logIndex =>
          IO:
            assert(logIndex.files.toSet ==
              Set(startInstant, startInstant + 24.h).map(instantToFile))
          *>
            logIndex.keyedByteLogLineStream(startInstant, LogSelection())
              .map(_.byteLine.utf8String)
              .compile.toList.map: lines =>
                assert(lines == List(
                  "2026-03-01 00:00:00.000+02 info LogDirectoryIndexTest - MESSAGE\n",
                  "2026-03-02 00:00:00.000+02 Begin ...\n",
                  "2026-03-02 00:00:00.000+02 info LogDirectoryIndexTest - MESSAGE\n"))
          *>
            IO:
              writeFile(startInstant + 48.h)
              awaitAndAssert(logIndex.files ==
                Seq(startInstant, startInstant + 24.h, startInstant + 48.h).map(instantToFile))
          *>
            logIndex.keyedByteLogLineStream(startInstant, LogSelection())
              .map(_.byteLine.utf8String)
              .compile.toList.map: lines =>
                assert(lines == List(
                  "2026-03-01 00:00:00.000+02 info LogDirectoryIndexTest - MESSAGE\n",
                  "2026-03-02 00:00:00.000+02 Begin ...\n",
                  "2026-03-02 00:00:00.000+02 info LogDirectoryIndexTest - MESSAGE\n",
                  "2026-03-03 00:00:00.000+02 Begin ...\n",
                  "2026-03-03 00:00:00.000+02 info LogDirectoryIndexTest - MESSAGE\n"))

  "Five 100MB debug-log files" in {
    if !isIntelliJIdea && !sys.props.contains("test.speed") then
      IO.pure(pending)
    else
      given ZoneId = ZoneId.of("Europe/Mariehamn")

      def info_(line: String) =
        logger.info(line)
        if !isIntelliJIdea then
          info(line)
          //println(s"➤LogFileIndex: $line")

      val logFileSize = 100 * 1024 * 1024
      val lineLength = 130
      val lineCount = logFileSize / lineLength
      temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
        (1 to 5).foldMap: i =>
          val date = s"2026-05-1$i"
          val gzFile = dir / s"TEST-debug-$date-1.log.gz"
          LogFileIndexTest.writeFile(
            gzFile, lineLength = lineLength, lineCount = lineCount, gzip = true,
            startTime = s"${date}T00:00:00.000+02")
        *>
          (1 to 10).foldMap: _ =>
            IO.defer:
              val t = Deadline.now
              given LogFileIndexConf = LogFileIndexConf.forTest
              given Config = ConfigFactory.empty()
              LogDirectoryIndex.resource(dir, logFilePrefixes = Set("TEST")).use: logDirectoryIndex =>
                logDirectoryIndex.logIndex(logFilePrefix = "TEST", Info).flatMap: logIndex =>
                  logIndex.byteLineStream(
                    Instant.parse("2026-02-12T00:01:00Z"),
                    LogSelection()
                  ).compile.drain.map: _ =>
                    val elapsed = t.elapsed
                    val used = sys.runtime.totalMemory - sys.runtime.freeMemory
                    info_(s"$logIndex ${
                      bold(bytesPerSecondString(elapsed, lineCount * lineLength))}")
            .as(succeed)
  }


private object LogDirectoryIndexTest:
  private val logger = Logger[this.type]
  private val headerTimestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSX")
  private val timestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSX")
