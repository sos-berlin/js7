package js7.base.log.reader

import cats.effect.{Deferred, IO, Resource}
import fs2.Stream
import java.io.FileOutputStream
import java.time.{Instant, ZoneId}
import java.util.zip.GZIPOutputStream
import js7.base.config.Js7Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.reader.LogIndexBuilder.LogFileAdded
import js7.base.problem.ProblemException
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.TimeoutException

final class LogIndexBuilderTest extends OurAsyncTestSuite:

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  "DelayedLogFile" - {
    given LogIndexConf =
      LogIndexConf.fromConfig:
        config"""
          js7.log.index.read-timestamp-tries  = [10ms, 30ms, 60ms, 100ms]
        """.withFallback(Js7Config.defaultConfig)
      .orThrow

    ".log-file is slowly written" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val file = dir / "test.log"
        IO:
          file :=
            """2026-06-25T00:00:00,111+03 Begin JS7 ...
              |2026-06-25T00:00:00,999+03 info  js7.journal.Journal - ...\n"""
            .stripMargin
        .delayBy(100.ms).background.surround:
          LogIndexBuilder.forTest(dir).use: logIndexBuilder =>
            val deferred = Deferred.unsafe[IO, LogFile | Throwable]
            logIndexBuilder.DelayedLogFile(file)
              .start(
                onCompleted = deferred.complete(_).void,
                onFailed = deferred.complete(_).void)
              .productR:
                deferred.get.map:
                  case logFile: LogFile =>
                    assert(logFile.originalFile == file
                      && logFile.fileInstant == Instant.parse("2026-06-25T00:00:00.999+03:00"))
                  case t: Throwable => throw t

    "First log line in .log.gz file is not written in time" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val file = dir / "test-2026-06-25-1.log.gz"
        Resource.fromAutoCloseable:
          IO(GZIPOutputStream(FileOutputStream(file.toFile)))
        .use_
        .productR:
          LogIndexBuilder.forTest(dir).use: logIndexBuilder =>
            val deferred = Deferred.unsafe[IO, LogFile | Throwable]
            logIndexBuilder.DelayedLogFile(file)
              .start(
                onCompleted = deferred.complete(_).void,
                onFailed = deferred.complete(_).void)
              .productR:
                deferred.get.map:
                  case t: TimeoutException => succeed
                  case t: Throwable => throw t
                  case _: LogFile => fail("Unexpected LogFile")

    "Invalid timestamp in a complete first log line fails fast (is not retried)" in:
      // Tries are long: A retry would not complete within the timeout of this test.
      given LogIndexConf =
        LogIndexConf.fromConfig:
          config"""
            js7.log.index.read-timestamp-tries  = [5s, 5s]
          """.withFallback(Js7Config.defaultConfig)
        .orThrow
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val file = dir / "test.log"
        file :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+99 info  js7.test.Test - ...
            |""".stripMargin
        LogIndexBuilder.forTest(dir).use: logIndexBuilder =>
          val deferred = Deferred.unsafe[IO, LogFile | Throwable]
          logIndexBuilder.DelayedLogFile(file)
            .start(
              onCompleted = deferred.complete(_).void,
              onFailed = deferred.complete(_).void)
            .productR:
              deferred.get.timeout(2.s).map:
                case _: ProblemException => succeed
                case o => fail(s"Unexpected: $o")
  }

  "toLogFileEvents" - {
    given LogIndexConf =
      LogIndexConf.fromConfig:
        config"""
          js7.log.index.read-timestamp-tries  = [10ms, 30ms, 60ms, 100ms]
        """.withFallback(Js7Config.defaultConfig)
      .orThrow

    "File with invalid timestamp is dropped, valid file is kept" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val valid = dir / "test-2026-06-25-1.log"
        val invalid = dir / "test-2026-06-25-2.log"
        valid :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.test.Test - ...
            |""".stripMargin
        // Complete first log line (ends with \n), but invalid timestamp: not an incomplete file
        invalid :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+99 info  js7.test.Test - ...
            |""".stripMargin
        LogIndexBuilder.toLogFileEvents(dir, Seq(valid, invalid)).use: (logFiles, _) =>
          IO:
            assert(logFiles.map(_.originalFile) == Seq(valid))

    "File with invalid timestamp is not delayed, no LogFileAdded follows" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val invalid = dir / "test-2026-06-25-1.log"
        invalid :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+99 info  js7.test.Test - ...
            |""".stripMargin
        LogIndexBuilder.toLogFileEvents(dir, Seq(invalid)).use: (logFiles, pipe) =>
          pipe(Stream.never[IO]).take(1).compile.toList
            .timeoutTo(500.ms, IO.pure(Nil)) // Longer than the sum of the tries
            .map: events =>
              assert(logFiles.isEmpty && events.isEmpty)

    "File with incomplete first log line is delayed, not dropped" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val incomplete = dir / "test-2026-06-25-1.log"
        // No \n after the first log line: log4j may still be writing it
        incomplete :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,99""".stripMargin
        LogIndexBuilder.toLogFileEvents(dir, Seq(incomplete)).use: (logFiles, pipe) =>
          IO(incomplete.append("9+03 info  js7.test.Test - ...\n"))
            .delayBy(50.ms).background.surround:
              pipe(Stream.never[IO]).take(1).compile.toList.timeout(5.s).map: events =>
                assert(logFiles.isEmpty &&
                  events.collect { case LogFileAdded(o) => o.originalFile } == List(incomplete))
  }
