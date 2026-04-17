package js7.tests.controller.proxy.log

import cats.effect.IO
import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.zip.GZIPOutputStream
import js7.base.config.{Js7Conf, Js7Config}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.LogLevel.{Debug, Info}
import js7.base.log.Logger
import js7.base.log.reader.{LogDirectoryIndex, LogFileIndexTest}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTime.extensions.+
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.AutoClosing.syntax.use
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.isIntelliJIdea
import js7.tester.ScalaTestUtils
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.controller.proxy.log.LogDirectoryIndexTest.*
import scala.concurrent.duration.Deadline

final class LogDirectoryIndexTest extends OurAsyncTestSuite:

  override def resourceForIORuntime =
    super.resourceForIORuntime.flatMap: _ =>
      Js7Conf.registerInEnvironment(Js7Config.defaultConfig)

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  "A continuous stream of log lines in all files" in:
    temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
      val startInstant = ZonedDateTime.parse("2026-03-01T00:00:00.000+02").toInstant
      IO:
        var i = 0
        (0 until 3).foreach: d =>
          val midnight = startInstant + 24.h * d
          (0 until 3).foreach: h =>
            val hour = midnight + h.h
            val gzFile = dir / s"js7-${hour.atZone(zoneId).toLocalDate}-$h.log.gz"
            autoClosing(
              new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(gzFile.toFile)))
            ): out =>
              out.write:
                (headerTimestampFormatter.format(hour.atZone(zoneId)) + " HEADER\n").getBytes(UTF_8)
              (1 to 3).foreach: s =>
                i += 1
                out.write:
                  s"${timestampFormatter.format((hour + s.s).atZone(zoneId))} info LogDirectoryIndexTest - MESSAGE $i\n"
                    .getBytes(UTF_8)
      .productR:
        LogDirectoryIndex.directory(dir, Info, _ => true, Js7Config.defaultConfig)
          .use: logDirectoryIndex =>
            logDirectoryIndex.instantToKeyedByteLogLineStream(startInstant, byteChunkSize = 8192)
              .map(_.byteLine.utf8String)
              .compile.toList.map: lines =>
                assert(lines == List(
                //"2026-03-01 00:00:00.000+0200 HEADER\n",
                  "2026-03-01 00:00:01.000 info LogDirectoryIndexTest - MESSAGE 1\n",
                  "2026-03-01 00:00:02.000 info LogDirectoryIndexTest - MESSAGE 2\n",
                  "2026-03-01 00:00:03.000 info LogDirectoryIndexTest - MESSAGE 3\n",

                //"2026-03-01 01:00:00.000+0200 HEADER\n",
                  "2026-03-01 01:00:01.000 info LogDirectoryIndexTest - MESSAGE 4\n",
                  "2026-03-01 01:00:02.000 info LogDirectoryIndexTest - MESSAGE 5\n",
                  "2026-03-01 01:00:03.000 info LogDirectoryIndexTest - MESSAGE 6\n",

                //"2026-03-01 02:00:00.000+0200 HEADER\n",
                  "2026-03-01 02:00:01.000 info LogDirectoryIndexTest - MESSAGE 7\n",
                  "2026-03-01 02:00:02.000 info LogDirectoryIndexTest - MESSAGE 8\n",
                  "2026-03-01 02:00:03.000 info LogDirectoryIndexTest - MESSAGE 9\n",

                //"2026-03-02 00:00:00.000+0200 HEADER\n",
                  "2026-03-02 00:00:01.000 info LogDirectoryIndexTest - MESSAGE 10\n",
                  "2026-03-02 00:00:02.000 info LogDirectoryIndexTest - MESSAGE 11\n",
                  "2026-03-02 00:00:03.000 info LogDirectoryIndexTest - MESSAGE 12\n",

                //"2026-03-02 01:00:00.000+0200 HEADER\n",
                  "2026-03-02 01:00:01.000 info LogDirectoryIndexTest - MESSAGE 13\n",
                  "2026-03-02 01:00:02.000 info LogDirectoryIndexTest - MESSAGE 14\n",
                  "2026-03-02 01:00:03.000 info LogDirectoryIndexTest - MESSAGE 15\n",

                //"2026-03-02 02:00:00.000+0200 HEADER\n",
                  "2026-03-02 02:00:01.000 info LogDirectoryIndexTest - MESSAGE 16\n",
                  "2026-03-02 02:00:02.000 info LogDirectoryIndexTest - MESSAGE 17\n",
                  "2026-03-02 02:00:03.000 info LogDirectoryIndexTest - MESSAGE 18\n",

                //"2026-03-03 00:00:00.000+0200 HEADER\n",
                  "2026-03-03 00:00:01.000 info LogDirectoryIndexTest - MESSAGE 19\n",
                  "2026-03-03 00:00:02.000 info LogDirectoryIndexTest - MESSAGE 20\n",
                  "2026-03-03 00:00:03.000 info LogDirectoryIndexTest - MESSAGE 21\n",

                //"2026-03-03 01:00:00.000+0200 HEADER\n",
                  "2026-03-03 01:00:01.000 info LogDirectoryIndexTest - MESSAGE 22\n",
                  "2026-03-03 01:00:02.000 info LogDirectoryIndexTest - MESSAGE 23\n",
                  "2026-03-03 01:00:03.000 info LogDirectoryIndexTest - MESSAGE 24\n",

                //"2026-03-03 02:00:00.000+0200 HEADER\n",
                  "2026-03-03 02:00:01.000 info LogDirectoryIndexTest - MESSAGE 25\n",
                  "2026-03-03 02:00:02.000 info LogDirectoryIndexTest - MESSAGE 26\n",
                  "2026-03-03 02:00:03.000 info LogDirectoryIndexTest - MESSAGE 27\n"))

  "Add a .log.gz" in:
    temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
      val startInstant = ZonedDateTime.parse("2026-03-01T00:00:00.000+02").toInstant
      def instantToFile(instant: Instant) =
        dir / s"js7-${instant.atZone(zoneId).toLocalDate}.log.gz"

      def writeFile(instant: Instant) =
        val gzFile = instantToFile(instant)
        new GZIPOutputStream(
          new BufferedOutputStream(new FileOutputStream(gzFile.toFile))
        ).use: out =>
          out.write:
            (headerTimestampFormatter.format(instant.atZone(zoneId)) + " HEADER\n").getBytes(UTF_8)
          out.write:
            s"${timestampFormatter.format(instant.atZone(zoneId))} info LogDirectoryIndexTest - MESSAGE\n"
              .getBytes(UTF_8)

      val aFile = instantToFile(startInstant)
      val bFile = instantToFile(startInstant + 24.h)
      writeFile(startInstant)
      writeFile(startInstant + 24.h)
      LogDirectoryIndex.directory(dir, Info, _ => true, Js7Config.defaultConfig)
        .use: logDirectoryIndex =>
          IO:
            assert:
              logDirectoryIndex.files == Seq(startInstant, startInstant + 24.h)
                .map(instantToFile)
          *>
            logDirectoryIndex.instantToKeyedByteLogLineStream(startInstant, byteChunkSize = 8192)
              .map(_.byteLine.utf8String)
              .compile.toList.map: lines =>
                assert(lines == List(
                  "2026-03-01 00:00:00.000 info LogDirectoryIndexTest - MESSAGE\n",
                  "2026-03-02 00:00:00.000 info LogDirectoryIndexTest - MESSAGE\n"))
          *>
            IO:
              writeFile(startInstant + 48.h)
              awaitAndAssert:
                logDirectoryIndex.files == Seq(startInstant, startInstant + 24.h, startInstant + 48.h)
                  .map(instantToFile)
          *>
            logDirectoryIndex.instantToKeyedByteLogLineStream(startInstant, byteChunkSize = 8192)
              .map(_.byteLine.utf8String)
              .compile.toList.map: lines =>
                assert(lines == List(
                  "2026-03-01 00:00:00.000 info LogDirectoryIndexTest - MESSAGE\n",
                  "2026-03-02 00:00:00.000 info LogDirectoryIndexTest - MESSAGE\n",
                  "2026-03-03 00:00:00.000 info LogDirectoryIndexTest - MESSAGE\n"))

  "1 GiB debug-log file" in {
    if !isIntelliJIdea && !sys.props.contains("test.speed") then
      IO.pure(pending)
    else
      given ZoneId = ZoneId.of("Europe/Mariehamn")

      def info_(line: String) =
        logger.info(line)
        if !isIntelliJIdea then
          println(s"➤LogFileIndex: $line")

      val logFileSize = 1024 * 1024 * 1024
      val lineLength = 130
      val lineCount = logFileSize / lineLength
      temporaryDirectoryResource[IO]("LogDirectoryIndexTest-").use: dir =>
        val gzFile = dir / "js7-debug-2026-03-21-3.log.gz"
        LogFileIndexTest.writeFile(
          gzFile, lineLength = lineLength, lineCount = lineCount, gzip = true
        ) *>
          (1 to 20).foldMap: _ =>
            IO.defer:
              val t = Deadline.now
              LogDirectoryIndex.directory(
                dir, Debug, isValidFile = _ => true, config = Js7Config.defaultConfig
              ).use: logDirectoryIndex =>
                logDirectoryIndex.instantToKeyedByteLogLineStream(
                  Instant.parse("2026-02-12T00:01:00Z"),
                  byteChunkSize = 8192
                ).take(1).compile.drain *>
                  IO:
                    val elapsed = t.elapsed
                    val used = sys.runtime.totalMemory - sys.runtime.freeMemory
                    // LogDirectoryIndex logs, too:
                    //info_(s"$logDirectoryIndex ${
                    //  bold(bytesPerSecondString(elapsed, lineCount * lineLength))}")
            .as(succeed)
  }


private object LogDirectoryIndexTest:
  private val logger = Logger[this.type]
  private val headerTimestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSXX")
  private val timestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
