package js7.data.event

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.event.EventRequestTest.*

/**
  * @author Joacim Zschimmer
  */
final class EventRequestTest extends OurTestSuite {

  "toQueryParameters" in {
    assert(EventRequest.singleClass[AEvent](after = EventId(3), timeout = Some(123.s), delay = 500.ms, limit = 999, tornOlder = Some(10.s))
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent",
          "delay" -> "0.5",
          "timeout" -> "123",
          "limit" -> "999",
          "tornOlder" -> "10",
          "after" -> "3"))
    assert(EventRequest[Event](
      Set[Class[? <: Event]](classOf[AEvent], classOf[BEvent]),
      after = EventId(3),
      timeout = Some(0.s),
      limit = Int.MaxValue)
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent,BEvent",
          "delay" -> "0",
          "timeout" -> "0",
          "after" -> "3"))
    assert(EventRequest.singleClass[AEvent](timeout = None)
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent",
          "delay" -> "0",
          "after" -> "0"))
  }
}

object EventRequestTest {
  private trait AEvent extends Event
  private trait BEvent extends Event
}
