package com.sos.scheduler.engine.data.event

import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class EventIdTest extends FreeSpec {

  private val MaximumJsonLosslessEventIdInstant = Instant.parse("2255-06-05T23:47:34.740992Z")

  "JsonMaxValue" in {
    assert(EventId.JsonMaxValue == 9007199254740992L)  // https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
    val perSecond = 1000 * EventId.IdsPerMillisecond
    val instant = Instant.ofEpochSecond(EventId.JsonMaxValue / perSecond, EventId.JsonMaxValue % perSecond * 1000)
    assert(instant == MaximumJsonLosslessEventIdInstant)
  }
}
