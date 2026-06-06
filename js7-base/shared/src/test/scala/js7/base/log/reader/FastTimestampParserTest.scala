package js7.base.log.reader

import java.nio.charset.StandardCharsets.UTF_8
import java.time.{Instant, ZoneId}
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.log.reader.FastTimestampParser.parseTimestampAsNanos
import js7.base.log.reader.FastTimestampParserTest.*
import js7.base.test.OurTestSuite
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Tests.isIntelliJIdea
import scala.concurrent.duration.Deadline

final class FastTimestampParserTest extends OurTestSuite:

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  private def toEpochNano(string: String) =
    // Java 17 does not accept "+02" and "+0200". It must be"+02:00"
    Instant.parse(string).toEpochNano

  "FastTimestampParser" in :
    val timestampParser = FastTimestampParser()
    assert(timestampParser.parse("1970-01-01 00:00:00.000Z") == EpochNano.Zero)
    assert(timestampParser.parse("1970-01-01 00:00:00.000123Z") == EpochNano(123_000))
    assert(timestampParser.parse("1970-01-01 00:00:00.000123+0100") == EpochNano(123_000 - 3600 * 1_000_000_000L))
    assert(timestampParser.parse("2026-02-25T01:02:03,123+0200") == toEpochNano("2026-02-25T01:02:03.123+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123456+0200") == toEpochNano("2026-02-25T21:22:23.123456+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123456+02:00") == toEpochNano("2026-02-25T21:22:23.123456+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123456+02") == toEpochNano("2026-02-25T21:22:23.123456+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123456+02") == toEpochNano("2026-02-25T21:22:23.123456+02:00"))

    assert(timestampParser.parse("2026-02-25 21:22:23.123456789-02:34") == toEpochNano("2026-02-25T21:22:23.123456789-02:34"))

    assert(timestampParser.parse("2026-02-25 21:22:23.123456789+02:34") == toEpochNano("2026-02-25T21:22:23.123456789+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.12345678+02:34") == toEpochNano("2026-02-25T21:22:23.12345678+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.1234567+02:34") == toEpochNano("2026-02-25T21:22:23.1234567+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123456+02:34") == toEpochNano("2026-02-25T21:22:23.123456+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.12345+02:34") == toEpochNano("2026-02-25T21:22:23.12345+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.1234+02:34") == toEpochNano("2026-02-25T21:22:23.1234+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123+02:34") == toEpochNano("2026-02-25T21:22:23.123+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.12+02:34") == toEpochNano("2026-02-25T21:22:23.12+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23.1+02:34") == toEpochNano("2026-02-25T21:22:23.1+02:34"))
    assert(timestampParser.parse("2026-02-25 21:22:23+02:34") == EpochNano.Nix)

    assert(timestampParser.parse("2026-02-25 21:22:23.123456789") == toEpochNano("2026-02-25T21:22:23.123456789+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.12345678") == toEpochNano("2026-02-25T21:22:23.12345678+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.1234567") == toEpochNano("2026-02-25T21:22:23.1234567+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123456") == toEpochNano("2026-02-25T21:22:23.123456+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.12345") == toEpochNano("2026-02-25T21:22:23.12345+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.1234") == toEpochNano("2026-02-25T21:22:23.1234+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.123") == toEpochNano("2026-02-25T21:22:23.123+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.12") == toEpochNano("2026-02-25T21:22:23.12+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23.1") == toEpochNano("2026-02-25T21:22:23.1+02:00"))
    assert(timestampParser.parse("2026-02-25 21:22:23") == toEpochNano("2026-02-25T21:22:23+02:00"))

    assert(timestampParser.parse("2222-12-12T12:00:00.654321+0200") == toEpochNano("2222-12-12T12:00:00.654321+02:00"))

  "FastTimestampParser, different seconds or millisecond" in:
    val timestampParser = FastTimestampParser()
    assert:
      timestampParser.parse("2026-02-12T12:00:00.000000+0300") ==
        toEpochNano("2026-02-12T12:00:00.000000+03:00")
    assert:
      timestampParser.parse("2026-02-12T12:00:00.000000+0300") ==
        toEpochNano("2026-02-12T09:00:00.000000Z")
    assert:
      timestampParser.parse("2026-02-12T12:00:00.000+0300") ==
        toEpochNano("2026-02-12T09:00:00.000000Z")
    assert:
      timestampParser.parse("2026-02-12T12:00:00.000000+0200") ==
        toEpochNano("2026-02-12T12:00:00.000000+02:00")
    assert:
      timestampParser.parse("2026-02-12T12:00:00.000001+0200") ==
        toEpochNano("2026-02-12T12:00:00.000001+02:00")
    assert:
      timestampParser.parse("2026-02-12T12:00:01.000001+0200") ==
        toEpochNano("2026-02-12T12:00:01.000001+02:00")
    assert:
      timestampParser.parse("2026-02-12T12:00:01.000001+02:00") ==
        toEpochNano("2026-02-12T12:00:01.000001+02:00")
    assert:
      timestampParser.parse("2026-02-12T12:00:01.000001+02") ==
        toEpochNano("2026-02-12T12:00:01.000001+02:00")
    assert:
      timestampParser.parse("2026-02-12T12:00:01.000001") ==
        toEpochNano("2026-02-12T12:00:01.000001+02:00")
    assert:
      timestampParser.parse("2222-12-12T12:00:00.000000+0200") ==
        toEpochNano("2222-12-12T12:00:00.000000+02:00")
    assert:
      timestampParser.parse("2222-12-12T12:00:00.000+0200") ==
        toEpochNano("2222-12-12T12:00:00.000000+02:00")
    assert:
      timestampParser.parse("2222-12-12T12:00:00.123456+0200") ==
        toEpochNano("2222-12-12T12:00:00.123456+02:00")
    assert:
      timestampParser.parse("2222-12-12T12:00:00.123+0200") ==
        toEpochNano("2222-12-12T12:00:00.123+02:00")

  "Speed" in:
    if isIntelliJIdea then
      val timestamp = "2026-02-12T12:00:00.123456+02" // Typical debug log line
      val timestampBytes = fs2.Chunk.array(timestamp.getBytes(UTF_8))
      (1 to 5).foreach: _ =>
        val timestampParser = FastTimestampParser()
        val t = Deadline.now
        val n = 100_000_000
        var i = 0
        while i < n do
          timestampParser.parse(timestampBytes)
          i += 1
        logger.info(bold(s"Optimized    : ${itemsPerSecondString(t.elapsed, n, "calls")}"))

      (1 to 3).foreach: _ =>
        val t = Deadline.now
        val n = 1_000_000
        (0 until n).foreach: _ =>
          parseTimestampAsNanos(timestamp)
        logger.info(bold(s"Not optimized: ${itemsPerSecondString(t.elapsed, n, "calls")}"))


object FastTimestampParserTest:
  private val logger = Logger[this.type]
