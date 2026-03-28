package js7.base.log.reader

import cats.effect.IO
import cats.effect.std.Queue
import java.nio.file.Files
import java.nio.file.Files.deleteIfExists
import java.time.{Instant, ZoneId, ZonedDateTime}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.log.reader.LogFileReader.{UniqueHeaderSize, growingLogFileStream, matchTimestampInLogLine, parseTimestampInHeaderLine, parseTimestampInLogLine}
import js7.base.log.reader.LogFileReaderTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTimeExtensions.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.{EpochNano, Stopwatch}
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.isIntelliJIdea
import org.scalatest.compatible.Assertion
import scala.util.Random

final class LogFileReaderTest extends OurAsyncTestSuite:

  "matchTimestampInLogLine" - {
    "matchTimestampInLogLine" in:
      assert:
        matchTimestampInLogLine:
          "2026-02-24 12:34:56.789 info [thread ] com.example.Example - Hello World!"
        .nn == "2026-02-24 12:34:56.789"
      assert:
        matchTimestampInLogLine:
          "2026-02-24 12:34:56 INFO com.example.Example - Hello World!"
        == null // because milliseconds are required
      assert:
        matchTimestampInLogLine:
          bold("2026-02-24T12:34:56,123456 ERROR Logger -")
        .nn == "2026-02-24T12:34:56,123456"

    "Speed" in:
      if !isIntelliJIdea then
        pending
      else
        val line = bold:
          "2026-02-24 12:34:56.789 info [thread] com.example.Example - Hello " + "." * 100
        val n = 1_000_000

        //locally:
        //  assert(matchTimestampInLogLineRaw(line).nn == (4, 4 + 23))
        //  val result = Stopwatch.measureTime(n, warmUp = n / 10):
        //    matchTimestampInLogLineRaw(line)
        //  logger.info(s"matchTimestampInLogLineRaw $result")

        locally:
          assert(matchTimestampInLogLine(line).nn == "2026-02-24 12:34:56.789")
          val result = Stopwatch.measureTime(n, warmUp = n / 10):
            matchTimestampInLogLine(line)
          logger.info(s"matchTimestampInLogLine $result")
        succeed
  }

  "parseTimestampInLogLine" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    val line = bold:
      "2026-02-24 12:34:56.789 info [thread] com.example.Example - Hello " + "." * 100
    val timestampParser = FastTimestampParser()
    assert:
      parseTimestampInLogLine(line)(timestampParser.parse(_)) ==
        ZonedDateTime.parse("2026-02-24T12:34:56.789+02").toInstant.toEpochNano

  "parseTimestampInHeaderLine" in:
    given ZoneId = ZoneId.of("Europe/Mariehamn")
    val toEpochNano = FastTimestampParser()
    locally:
      val line = bold("2026-02-24T08:05:55.244Z Begin JS7 Test · 2.9.0-SNAPSHOT")
      assert:
        parseTimestampInHeaderLine(line) ==
          ZonedDateTime.parse("2026-02-24T08:05:55.244Z").toInstant.toEpochNano

    locally:
      val line = bold("2026-02-24T08:05:55.244+02:00 Begin JS7 Test · 2.9.0-SNAPSHOT")
      assert:
        parseTimestampInHeaderLine(line) ==
          ZonedDateTime.parse("2026-02-24T08:05:55.244+02").toInstant.toEpochNano

    locally:
      val line = bold("2026-02-24 08:05:55,244+0200 Begin JS7 Test · 2.9.0-SNAPSHOT")
      assert:
        parseTimestampInHeaderLine(line) ==
          ZonedDateTime.parse("2026-02-24T08:05:55.244+02").toInstant.toEpochNano

    locally:
      val line = bold("2026-02-24 08:05:55.244272+02:00 Begin JS7 Test · 2.9.0-SNAPSHOT")
      assert:
        parseTimestampInHeaderLine(line) ==
          Instant.parse("2026-02-24T06:05:55.244272Z").toEpochNano

    locally:
      val line = bold("2026-02-24 08:05:55.244272+02:00 Begin JS7 Test · 2.9.0-SNAPSHOT")
      assert:
        parseTimestampInHeaderLine(line) ==
          ZonedDateTime.parse("2026-02-24T08:05:55.244272+02").toInstant.toEpochNano

  "growingLogFileStream" in :
    temporaryFileResource[IO]("LogFileReaderTest").use: file =>
      (1 to 2).foldMap: i =>
        IO.defer:
          logger.debug(s"——— $i " * 10)
          deleteIfExists(file)
          file := ""
          Queue.unbounded[IO, ByteArray].flatMap: queue =>
            def writeAndRead(key: String): IO[Assertion] =
              IO.sleep(100.ms) *>
                (1 to 20).foldMap: j =>
                  IO.defer:
                    val more = ByteArray:
                      key + " " +
                        (s"loop=$i index=$j " + "." * UniqueHeaderSize).take(UniqueHeaderSize) +
                        (1 to Random.nextInt(10)).toVector.map(i => Random.nextPrintableChar()).mkString +
                        "\n"
                    assert(more.length >= UniqueHeaderSize)
                    file ++= more
                    queue.take.timeout(9.s).map: chunk =>
                      assert(chunk == more)

            growingLogFileStream[ByteArray](file, byteChunkSize = 1024, pollDuration = 10.ms)
              .takeWhile(_ != Stop)
              .foreach(queue.offer)
              .compile.drain
              .both:
                writeAndRead("A") // First log file //
                  .productL:
                    // Change log file //
                    IO.blocking:
                      logger.info("Changing log file")
                      Files.delete(file)
                  .productL:
                    writeAndRead("B") // Second log file //
                  .productL:
                    IO:
                      file := Stop
              .map(_._2)


object LogFileReaderTest:
  private val logger = Logger[this.type]
  private val Stop = ByteArray("STOP" + "." * UniqueHeaderSize)
