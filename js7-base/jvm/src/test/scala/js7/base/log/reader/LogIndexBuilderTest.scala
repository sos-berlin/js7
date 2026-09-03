package js7.base.log.reader

import cats.effect.{Deferred, IO, Resource}
import java.io.FileOutputStream
import java.time.{Instant, ZoneId}
import java.util.zip.GZIPOutputStream
import js7.base.config.Js7Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
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

    "First log line in .log.gz-file is not written in time" in:
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
  }
