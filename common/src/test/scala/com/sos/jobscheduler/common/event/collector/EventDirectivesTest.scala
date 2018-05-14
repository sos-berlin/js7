package com.sos.jobscheduler.common.event.collector

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.event.collector.EventDirectivesTest._
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent}
import io.circe.generic.JsonCodec
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventDirectivesTest extends FreeSpec with ScalatestRouteTest {

  private implicit val myKeyedEventJsonFormat = {
    KeyedEvent.typedJsonCodec[MyEvent](
      KeyedSubtype.singleEvent[AEvent],
      KeyedSubtype.singleEvent[BEvent])
  }

  private def route =
    path("test") {
      eventRequest[MyEvent].apply { eventRequest ⇒
        if (eventRequest == EventRequest[MyEvent](Set(classOf[AEvent]),
            after = EventId(66), delay = 770.millis, timeout = 88.seconds, limit = 99))
          complete("A")
        else
        if (eventRequest == EventRequest[MyEvent](Set(classOf[AEvent], classOf[BEvent]),
            after = EventId(666), delay = 777.millis, timeout = 888.seconds, limit = 999))
          complete("B")
        else {
          println(eventRequest)
          reject
        }
      }
    }

  "eventRequest" in {
    Get("/test?return=AEvent&delay=0.77&timeout=88&limit=99&after=66") ~> route ~> check {
      assert(responseAs[String] == "A")
    }
    Get("/test?return=AEvent,BEvent&delay=0.777&timeout=888&limit=999&after=666") ~> route ~> check {
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
