package js7.base.log.reader

import cats.effect.{Deferred, IO}
import java.time.{Instant, ZoneId}
import js7.base.config.Js7Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.reader.LogDirectoryIndex.LogFile
import js7.base.log.reader.recompressors.LogFileIndexConf
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.TimeoutException

final class LogDirectoryIndexBuilderTest extends OurAsyncTestSuite:

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  "DelayedLogFile" - {
    given LogFileIndexConf =
      LogFileIndexConf.fromConfig:
        config"""js7.log.read-timestamp-tries = [10ms, 30ms, 60ms]"""
          .withFallback(Js7Config.defaultConfig)
      .orThrow

    ".log-file is slowly written" in:
      temporaryDirectoryResource[IO]("LogDirectoryIndexBuilderTest-").use: dir =>
        val file = dir / "test.log"
        file :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.journal.Journal - ...""".stripMargin
        sleep(10.ms)
        file ++= "\n"
        LogDirectoryIndexBuilder.forTest(dir).use: builder =>
          val deferred = Deferred.unsafe[IO, LogFile | Throwable]
          builder.DelayedLogFile(file)
            .start(
              onCompleted = deferred.complete(_).void,
              onFailed = deferred.complete(_).void)
            .productR:
              deferred.get.map:
                case logFile: LogFile =>
                  assert(logFile.originalFile == file
                    && logFile.fileInstant == Instant.parse("2026-06-25T00:00:00.999+03:00"))
                case t: Throwable => throw t

    ".log-file is not written in time" in:
      temporaryDirectoryResource[IO]("LogDirectoryIndexBuilderTest-").use: dir =>
        val file = dir / "test.log"
        file :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.journal.Journal - ...""".stripMargin
        LogDirectoryIndexBuilder.forTest(dir).use: builder =>
          val deferred = Deferred.unsafe[IO, LogFile | Throwable]
          builder.DelayedLogFile(file)
            .start(
              onCompleted = deferred.complete(_).void,
              onFailed = deferred.complete(_).void)
            .productR:
              deferred.get.map:
                case t: TimeoutException => succeed
                case t: Throwable => throw t
                case _: LogFile => fail("Unexpected LogFile")
  }
