package js7.base.log.reader

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.parallel.*
import fs2.Stream
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.time.{Instant, ZoneId}
import java.util.zip.GZIPOutputStream
import js7.base.config.Js7Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.io.file.watch.DirectoryEvent
import js7.base.io.file.watch.DirectoryEvent.FileAdded
import js7.base.log.reader.LogIndexBuilder.LogFilePending
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.Using

final class LogIndexBuilderTest extends OurAsyncTestSuite:

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")
  private given LogIndexConf = LogIndexConf.forTest

  "toLogFilesAndEvents" - {
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
        LogIndexBuilder.toLogFilesAndEvents(dir, Seq(valid, invalid)).map: (logFiles, pending, _) =>
          assert(logFiles.map(_.originalFile) == Seq(valid) && pending.isEmpty)

    "File with incomplete first log line is pending, not dropped" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val incomplete = dir / "test-2026-06-25-1.log"
        // No \n after the first log line: log4j may still be writing it
        incomplete :=
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,99""".stripMargin
        LogIndexBuilder.toLogFilesAndEvents(dir, Seq(incomplete)).map: (logFiles, pending, _) =>
          assert(logFiles.isEmpty && pending == Seq(incomplete))

    "FileAdded is converted to LogFilePending without reading the file" in:
      temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
        val file = dir / "test.log" // Does not exist
        LogIndexBuilder.toLogFilesAndEvents(dir, Nil).flatMap: (_, _, pipe) =>
          pipe(Stream.emit(FileAdded(file.getFileName))).compile.toList.map: events =>
            assert(events == List(LogFilePending(file)))
  }

  "LogIndex reads the timestamp of a pending file when it is accessed" in:
    temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
      val file = dir / "test.log"
      file :=
        """2026-06-25T00:00:00,111+03 Begin JS7 ...
          |2026-06-25T00:00:00,99""".stripMargin
      LogIndex.directory(dir, Seq(file), Stream.never[IO], watchGrowth = false, label = "TEST")
        .use: logIndex =>
          val instant = Instant.parse("2026-06-25T00:00:00.999+03:00")
          for
            // Still incomplete, the file stays pending
            key0 <- logIndex.instantToLogLineKey(instant, LogSelection())
            _ <- IO:
              file.append("9+03 info  js7.test.Test - ...\n")
            // Now the first log line is complete. No polling has happened, the access reads it.
            lines <- logIndex.keyedByteLogLineStream(instant, LogSelection()).compile.toList
          yield
            assert(key0 == None
              && lines.nonEmpty
              && lines.forall(_.logLineKey.fileInstant == instant)
              && logIndex.files == Seq(file))

  "Growing readers waiting for the next file get it when its first log line is completed" in:
    given LogIndexConf =
      LogIndexConf.fromConfig:
        config"""
          js7.log.index.read-timestamp-tries  = [10ms, 30ms, 60ms, 100ms]
        """.withFallback(Js7Config.defaultConfig)
      .orThrow

    temporaryDirectoryResource[IO]("LogIndexBuilderTest-").use: dir =>
      val a = dir / "test-2026-06-25-1.log.gz"
      val b = dir / "test-2026-06-25-2.log"
      Using.resource(GZIPOutputStream(FileOutputStream(a.toFile))): out =>
        out.write:
          """2026-06-25T00:00:00,111+03 Begin JS7 ...
            |2026-06-25T00:00:00,999+03 info  js7.test.Test - A
            |""".stripMargin.getBytes(UTF_8)
      // Incomplete first log line, the file is not yet readable
      b :=
        """2026-06-25T01:00:00,111+03 Begin JS7 ...
          |2026-06-25T01:00:00,99""".stripMargin
      Queue.unbounded[IO, Option[DirectoryEvent]].flatMap: queue =>
        LogIndex.directory(dir, Seq(a), Stream.fromQueueNoneTerminated(queue),
          watchGrowth = true, label = "TEST"
        ).use: logIndex =>
          val follower =
            logIndex
              .stringLineStream(
                Instant.parse("2026-06-24T00:00:00Z"),
                LogSelection(growing = true))
              .take(4) // 2 lines from a, 2 lines from b
              .compile.toList
              .timeout(10.s)
          // Two readers share one poller
          (follower, follower).parTupled.background.use: readers =>
            for
              // The readers have consumed a and now wait for the next file
              _ <- IO.sleep(300.ms)
              _ <- queue.offer(Some(FileAdded(b.getFileName)))
              _ <- IO.sleep(300.ms)
              _ <- IO(b.append("9+03 info  js7.test.Test - B\n"))
              outcome <- readers
              (lines1, lines2) <- outcome.embedError
            yield
              val expected = List(
                "2026-06-25T00:00:00,111+03 Begin JS7 ...",
                "2026-06-25T00:00:00,999+03 info  js7.test.Test - A",
                "2026-06-25T01:00:00,111+03 Begin JS7 ...",
                "2026-06-25T01:00:00,999+03 info  js7.test.Test - B")
              assert(lines1.map(_.trim) == expected && lines2.map(_.trim) == expected)
