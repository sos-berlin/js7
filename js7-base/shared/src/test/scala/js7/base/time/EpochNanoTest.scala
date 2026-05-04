package js7.base.time

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class EpochNanoTest extends OurTestSuite:

  "JSON" in:
    testJson(EpochNano(99_123_456_789L), json"99.123456789")
    testJson(EpochNano(99_123_000_000L), json"99.123")
    testJson(EpochNano(99_000_000_000L), json"99")
    testJson(EpochNano(1), json"0.000000001")
    testJson(EpochNano(0L), json"0")
    testJson(EpochNano(-1), json"-0.000000001")

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
