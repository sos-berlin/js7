package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.collector.EventDirectives.eventRequest
import com.sos.scheduler.engine.common.event.collector.EventDirectivesTest._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.scheduler.engine.data.event.{Event, EventId, EventRequest, KeyedEvent}
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class EventDirectivesTest extends FreeSpec with ScalatestRouteTest {

  private implicit val myKeyedEventJsonFormat = {
    implicit val a = jsonFormat0(AEvent)
    implicit val b = jsonFormat0(BEvent)
    KeyedEvent.typedJsonFormat[MyEvent](
      KeyedSubtype[AEvent],
      KeyedSubtype[BEvent])
  }

  "eventRequest" in {
    def route =
      path("test") {
        eventRequest[MyEvent].apply { eventRequest ⇒
          if (eventRequest == EventRequest[MyEvent](Set(classOf[AEvent]), after = EventId(7), 60.s, limit = 999))
            complete("A")
          else
          if (eventRequest == EventRequest[MyEvent](Set(classOf[AEvent], classOf[BEvent]), after = EventId(777), 60.s, limit = 999))
            complete("B")
          else
            reject
        }
      }
    Get("/test?return=AEvent&timeout=60&limit=999&after=7") ~> route ~> check {
      assert(responseAs[String] == "A")
    }
    Get("/test?return=AEvent,BEvent&timeout=60&limit=999&after=777") ~> route ~> check {
      assert(responseAs[String] == "B")
    }
  }
}

object EventDirectivesTest {
  private sealed trait MyEvent extends Event {
    type Key = String
  }
  private case class AEvent() extends MyEvent
  private case class BEvent() extends MyEvent
}
