package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.data.event.EventRequestTest._
import java.time.Duration
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventRequestTest extends FreeSpec {

  "toQueryParameters" in {
    assert(EventRequest.singleClass[AEvent](after = EventId(3), timeout = Duration.ofSeconds(123), delay = Duration.ofMillis(500), limit = 999)
      .toQueryParameters ==
        Vector(
          "return" → "AEvent",
          "timeout" → "PT2M3S",
          "delay" → "PT0.5S",
          "limit" → "999",
          "after" → "3"))
    assert(EventRequest[Event](
      Set[Class[_ <: Event]](classOf[AEvent], classOf[BEvent]),
      after = EventId(3),
      timeout = Duration.ZERO,
      limit = Int.MaxValue)
      .toQueryParameters ==
        Vector(
          "return" → "AEvent,BEvent",
          "delay" → "PT0S",
          "after" → "3"))
    assert(ReverseEventRequest[AEvent](after = EventId(3), limit = 999).toQueryParameters ==
      Vector(
        "return" → "AEvent",
        "limit" → "-999",
        "after" → "3"))
  }
}

object EventRequestTest {

  private trait AEvent extends Event
  private trait BEvent extends Event
}
