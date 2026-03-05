package js7.base.log.reader

import java.time.{Instant, ZoneId}
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.log.reader.FastTimestampParser.parseTimestampAsNanos
import js7.base.log.reader.FastTimestampParserTest.*
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimeExtensions.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Tests.isIntelliJIdea
import scala.concurrent.duration.Deadline

final class FastTimestampParserTest extends OurTestSuite:

  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  private def toEpochNano(string: String) =
    Instant.parse(string).toEpochNano

  "FastTimestampParser" in :
    val toNanos = new FastTimestampParser()
    assert(toNanos("2026-02-25T01:02:03,123") == toEpochNano("2026-02-25T01:02:03.123+02:00"))
    assert(toNanos("2026-02-25 21:22:23.123456") == toEpochNano("2026-02-25T21:22:23.123456+02:00"))

  "FastTimestampParser, different seconds or millisecond" in:
    val toNanos = FastTimestampParser()
    assert:
      toNanos("2026-02-12T12:00:00.000000") ==
        parseTimestampAsNanos("2026-02-12T12:00:00.000000")
    assert:
      toNanos("2026-02-12T12:00:00.000000") ==
        parseTimestampAsNanos("2026-02-12T12:00:00.000000")
    assert:
      toNanos("2026-02-12T12:00:00.000001") ==
        parseTimestampAsNanos("2026-02-12T12:00:00.000001")
    assert:
      toNanos("2026-02-12T12:00:01.000001") ==
        parseTimestampAsNanos("2026-02-12T12:00:01.000001")
    assert:
      toNanos("2999-12-12T12:00:00.000000") ==
        parseTimestampAsNanos("2999-12-12T12:00:00.000000")

  "Speed" in :
    if isIntelliJIdea then
      locally:
        val toNanos = FastTimestampParser()
        val t = Deadline.now
        (0 until 10_000_000).foreach: _ =>
          toNanos("2026-02-12T12:00:00.000000")
        logger.info(bold(s"Optimized    : ${itemsPerSecondString(t.elapsed, 1000000, "calls")}"))

      locally:
        val t = Deadline.now
        (0 until 10_000_000).foreach: _ =>
          parseTimestampAsNanos("2026-02-12T12:00:00.000000")
        logger.info(bold(s"Not optimized: ${itemsPerSecondString(t.elapsed, 1000000, "calls")}"))
    succeed


object FastTimestampParserTest:
  private val logger = Logger[this.type]
