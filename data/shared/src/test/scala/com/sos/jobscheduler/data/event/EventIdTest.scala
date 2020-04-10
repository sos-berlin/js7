package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.time.Timestamp
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventIdTest extends AnyFreeSpec
{
  private val MaximumJsonLosslessEventIdInstant = Timestamp.parse("2255-06-05T23:47:34.740Z")
//private val MaximumJsonLosslessEventIdInstant = Timestamp.parse("2255-06-05T23:47:34.740992Z")

  "JavascriptMaxValue" in {
    assert(EventId.JavascriptMaxValue == 9007199254740992L)  // https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
    val timestamp = Timestamp.ofEpochMilli(EventId.JavascriptMaxValue / 1000)
    assert(timestamp == MaximumJsonLosslessEventIdInstant)
  }

  "toString" in {
    assert(EventId.toString(EventId(      0)) ==       "0/BeforeFirst")
    assert(EventId.toString(EventId(      1)) ==       "1/1970-01-01T00:00:00.000Z.001")
    assert(EventId.toString(EventId( 123000)) ==  "123000/1970-01-01T00:00:00.123Z")
    assert(EventId.toString(EventId( 123400)) ==  "123400/1970-01-01T00:00:00.123Z.400")
    assert(EventId.toString(EventId( 123450)) ==  "123450/1970-01-01T00:00:00.123Z.450")
    assert(EventId.toString(EventId( 123456)) ==  "123456/1970-01-01T00:00:00.123Z.456")
    assert(EventId.toString(EventId(1000000)) == "1000000/1970-01-01T00:00:01Z")
    assert(EventId.toString(EventId.JavascriptMaxValue) == "9007199254740992/2255-06-05T23:47:34.740Z.992")
  }
}
