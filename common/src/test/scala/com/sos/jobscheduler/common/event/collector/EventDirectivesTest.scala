package js7.common.event.collector

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.common.event.collector.EventDirectives.eventRequest
import js7.common.event.collector.EventDirectivesTest._
import js7.base.time.ScalaTime._
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent}
import io.circe.generic.JsonCodec
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventDirectivesTest extends AnyFreeSpec with ScalatestRouteTest {

  private implicit val myKeyedEventJsonFormat = {
    KeyedEvent.typedJsonCodec[MyEvent](
      KeyedSubtype.singleEvent[AEvent],
      KeyedSubtype.singleEvent[BEvent])
  }

  private def route =
    path("test") {
      eventRequest[MyEvent].apply { eventReq =>
        if (eventReq == EventRequest[MyEvent](Set(classOf[AEvent]), after = EventId(1), delay = EventDirectives.DefaultDelay, timeout = Some(Duration.Zero)))
          complete("DEFAULT")
        else
        if (eventReq == EventRequest[MyEvent](Set(classOf[AEvent]), after = EventId(66), delay = 770.millis, timeout = Some(88.s), limit = 99, tornOlder = Some(10.s)))
          complete("A")
        else
        if (eventReq == EventRequest[MyEvent](Set(classOf[AEvent], classOf[BEvent]), after = EventId(666), delay = 777.millis, timeout = Some(888.s), limit = 999))
          complete("B")
        else
        if (eventReq == EventRequest[MyEvent](Set(classOf[AEvent], classOf[BEvent]), after = EventId(3), delay = EventDirectives.DefaultDelay, timeout = None))
          complete("C")
        else {
          println(eventReq)
          reject
        }
      }
    }

  "eventRequest" in {
    Get("/test?return=AEvent&after=1") ~> route ~> check {
      assert(responseAs[String] == "DEFAULT")
    }
    Get("/test?return=AEvent&delay=0.77&timeout=88&limit=99&after=66&tornOlder=10") ~> route ~> check {
      assert(responseAs[String] == "A")
    }
    Get("/test?return=AEvent,BEvent&delay=0.777&timeout=888&limit=999&after=666") ~> route ~> check {
      assert(responseAs[String] == "B")
    }
    Get("/test?return=AEvent,BEvent&timeout=infinite&after=3") ~> route ~> check {
      assert(responseAs[String] == "C")
    }
  }
}

object EventDirectivesTest {
  sealed trait MyEvent extends Event {
    type Key = String
  }
  @JsonCodec final case class AEvent() extends MyEvent
  @JsonCodec final case class BEvent() extends MyEvent
}
