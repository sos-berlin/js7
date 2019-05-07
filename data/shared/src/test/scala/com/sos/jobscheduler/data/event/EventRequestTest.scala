package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.data.event.EventRequestTest._
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventRequestTest extends FreeSpec {

  "toQueryParameters" in {
    assert(EventRequest.singleClass[AEvent](after = EventId(3), timeout = Some(123.seconds), delay = 500.milliseconds, limit = 999, tornOlder = Some(10.seconds))
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent",
          "timeout" -> "123",
          "delay" -> "0.5",
          "limit" -> "999",
          "tornOlder" -> "10",
          "after" -> "3"))
    assert(EventRequest[Event](
      Set[Class[_ <: Event]](classOf[AEvent], classOf[BEvent]),
      after = EventId(3),
      timeout = Some(Duration.Zero),
      limit = Int.MaxValue)
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent,BEvent",
          "after" -> "3"))
    assert(EventRequest.singleClass[AEvent](timeout = None)
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent",
          "timeout" -> "infinite",
          "after" -> "0"))
    assert(ReverseEventRequest[AEvent](after = EventId(3), limit = 999).toQueryParameters ==
      Vector(
        "return" -> "AEvent",
        "limit" -> "-999",
        "after" -> "3"))
  }
}

object EventRequestTest {
  private trait AEvent extends Event
  private trait BEvent extends Event
}
