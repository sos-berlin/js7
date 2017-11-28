package com.sos.jobscheduler.common.event.collector

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.event.collector.EventDirectivesTest._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent}
import io.circe.generic.JsonCodec
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventDirectivesTest extends FreeSpec with ScalatestRouteTest {

  private implicit val myKeyedEventJsonFormat = {
    KeyedEvent.typedJsonCodec[MyEvent](
      KeyedSubtype.singleEvent[AEvent],
      KeyedSubtype.singleEvent[BEvent])
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
  sealed trait MyEvent extends Event {
    type Key = String
  }
  @JsonCodec final case class AEvent() extends MyEvent
  @JsonCodec final case class BEvent() extends MyEvent
}
