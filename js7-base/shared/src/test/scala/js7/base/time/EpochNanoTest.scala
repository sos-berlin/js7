package js7.base.time

import io.circe.syntax.EncoderOps
import java.time.Instant
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.EpochNano.toEpochNano
import js7.tester.CirceJsonTester.testJson

final class EpochNanoTest extends OurTestSuite:

  "Range" in:
    assert(EpochNano(Long.MaxValue).toInstant == Instant.parse("2262-04-11T23:47:16.854775807Z"))
    assert(Instant.parse("2262-04-11T23:47:16.854775807Z").toEpochNano == EpochNano(Long.MaxValue))
    assert(EpochNano(0).toInstant == Instant.parse("1970-01-01T00:00:00Z"))

  "JSON" in:
    // Compatible with Timestamp's serialization as milliseconds (as usual in Java code)
    val ts = Timestamp("2026-05-18T12:34:56.789Z")
    assert(ts.toEpochNano == Instant.parse("2026-05-18T12:34:56.789Z").toEpochNano)
    assert(ts.toEpochNano.asJson == ts.asJson)

    testJson(EpochNano(99_123_456_789L), json"99123.456789")
    testJson(EpochNano(99_123_000_000L), json"99123")
    testJson(EpochNano(99_000_000_000L), json"99000")
    testJson(EpochNano(1), json"0.000001")
    testJson(EpochNano(0L), json"0")
    testJson(EpochNano(-1), json"-0.000001")

  "string" in:
    assert(EpochNano(99_123_456_789L).toDecimalString == "99.123456789")
    assert(EpochNano(99_123_000_000L).toDecimalString == "99.123")
    assert(EpochNano(99_000_000_000L).toDecimalString == "99")
    assert(EpochNano(1).toDecimalString == "0.000000001")
    assert(EpochNano(0L).toDecimalString == "0")
    assert(EpochNano(-1).toDecimalString == "-0.000000001")

    assert(EpochNano.fromDecimalString("99.123456789") == EpochNano(99_123_456_789L))
    assert(EpochNano.fromDecimalString("99.123") == EpochNano(99_123_000_000L))
    assert(EpochNano.fromDecimalString("99") == EpochNano(99_000_000_000L))
    assert(EpochNano.fromDecimalString("0.000000001") == EpochNano(1))
    assert(EpochNano.fromDecimalString("0") == EpochNano(0L))
    assert(EpochNano.fromDecimalString("-0.000000001") == EpochNano(-1))
