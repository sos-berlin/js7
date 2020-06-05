package js7.data.event

import js7.data.event.EventRequestTest._
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventRequestTest extends AnyFreeSpec {

  "toQueryParameters" in {
    assert(EventRequest.singleClass[AEvent](after = EventId(3), timeout = Some(123.seconds), delay = 500.milliseconds, limit = 999, tornOlder = Some(10.seconds))
      .toQueryParameters ==
        Vector(
          "return" -> "AEvent",
          "delay" -> "0.5",
          "timeout" -> "123",
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
