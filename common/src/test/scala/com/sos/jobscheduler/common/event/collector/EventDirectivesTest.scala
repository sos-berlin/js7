package com.sos.jobscheduler.common.event.collector

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.event.collector.EventDirectivesTest._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent}
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._

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
          if (eventRequest == EventRequest[MyEvent](Set(classOf[AEvent]), after = EventId(7), timeout = 60.s, limit = 999))
            complete("A")
          else
          if (eventRequest == EventRequest[MyEvent](Set(classOf[AEvent], classOf[BEvent]), after = EventId(777), 60.s, limit = 999))
            complete("B")
          else
            reject
        }
      }
    Get("/test?return=AEvent&timeout=60&delay=0&limit=999&after=7") ~> route ~> check {
      assert(responseAs[String] == "A")
    }
    Get("/test?return=AEvent,BEvent&timeout=60&delay=0&limit=999&after=777") ~> route ~> check {
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
