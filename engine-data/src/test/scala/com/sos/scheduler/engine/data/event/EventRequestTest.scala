package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.data.event.EventRequestTest._
import java.time.Duration
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class EventRequestTest extends FreeSpec {

  "toQueryParameters" in {
    assert(EventRequest(classOf[AEvent], after = EventId(3), timeout = Duration.ofSeconds(123), limit = 999).toQueryParameters ==
      Vector(
        "return" → "AEvent",
        "timeout" → "PT2M3S",
        "limit" → "999",
        "after" → "3"))
    assert(EventRequest(classOf[AEvent], after = EventId(3), timeout = Duration.ZERO, limit = Int.MaxValue).toQueryParameters ==
      Vector(
        "return" → "AEvent",
        "after" → "3"))
    assert(ReverseEventRequest(classOf[AEvent], after = EventId(3), limit = 999).toQueryParameters ==
      Vector(
        "return" → "AEvent",
        "limit" → "-999",
        "after" → "3"))
  }
}

object EventRequestTest {

  private trait AEvent extends Event
}
