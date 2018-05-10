package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.order.OrderFatEvent.{KeyedOrderFatEventJsonCodec, OrderAddedFat}
import com.sos.jobscheduler.data.order.{OrderFatEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRouteTest._
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class FatEventRouteTest extends FreeSpec with ScalatestRouteTest with FatEventRoute {

  private implicit val timeout = 9.seconds
  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventReader = new TestEventReader
  protected implicit def scheduler = Scheduler.global

  TestEvents foreach eventReader.addStamped

  private def route = pathSegments("fatEvent")(fatEventRoute)

  for (uri ← List(
    "/fatEvent?return=OrderFatEvent&timeout=60&after=0",
    "/fatEvent?after=0"))
  {
    s"$uri" in {
      assert(getFatEvents(uri) == TestFatEvents)
    }
  }

  "/fatEvent?limit=3&after=0 start" in {
    assert(getFatEvents("/fatEvent?limit=3&after=0") == fatEventsAfter(0).take(3))
    assert(eventReader.lastEventsAfter == 0)
  }

  "/fatEvent?limit=3&after=30 continue" in {
    val stampeds = getFatEvents("/fatEvent?limit=3&after=30")
    assert(stampeds == fatEventsAfter(30).take(3))
    assert(stampeds.head.eventId == 40)
    assert(stampeds.last.eventId == 60)
    assert(eventReader.lastEventsAfter == 30)
  }

  "/fatEvent?limit=3&after=60 continue" in {
    val stampeds = getFatEvents("/fatEvent?limit=3&after=60")
    assert(stampeds == fatEventsAfter(60).take(3))
    assert(stampeds.head.eventId == 70)
    assert(stampeds.last.eventId == 90)
    assert(eventReader.lastEventsAfter == 60)
  }

  "/fatEvent?limit=1&after=70 rewind in last chunk" in {
    val stampeds = getFatEvents("/fatEvent?limit=3&after=70")
    assert(stampeds == fatEventsAfter(70).take(3))
    assert(stampeds.head.eventId ==  80)
    assert(stampeds.last.eventId == 100)
    assert(eventReader.lastEventsAfter == 60)  // Rewound to _lastRequestedState
  }

  "/fatEvent?limit=3&after=80 continue" in {
    val stampeds = getFatEvents("/fatEvent?limit=3&after=80")
    assert(stampeds == fatEventsAfter(80).take(3))
    assert(stampeds.head.eventId ==  90)
    assert(stampeds.last.eventId == 110)
    assert(eventReader.lastEventsAfter == 70)  // Rewound to _lastRequestedState
  }

  "/fatEvent?limit=3&after=60 rewind to oldest" in {
    val stampeds = getFatEvents("/fatEvent?limit=3&after=60")
    assert(stampeds == fatEventsAfter(60).take(3))
    assert(stampeds.head.eventId == 70)
    assert(stampeds.last.eventId == 90)
    assert(eventReader.lastEventsAfter == eventReader.oldestEventId)  // Rewound to oldestEventId == BeforeFirst (no snapshot taken)
    assert(eventReader.lastEventsAfter == EventId.BeforeFirst)
  }

  "/fatEvent?limit=3&after=150 skip some events" in {
    val stampeds = getFatEvents("/fatEvent?limit=3&after=150")
    assert(stampeds == fatEventsAfter(150).take(3))
    assert(stampeds.head.eventId == 160)
    assert(stampeds.last.eventId == 180)
    assert(eventReader.lastEventsAfter == 90)  // Skipped until after 150
  }

  "After truncated journal snapshot" in pending  // TODO

  private def getFatEvents(uri: String): Seq[Stamped[KeyedEvent[OrderFatEvent]]] =
    Get(uri) ~> Accept(`application/json`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      val EventSeq.NonEmpty(stampeds) = responseAs[TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]]]
      stampeds
    }

  protected final class TestEventReader extends EventCollector(EventCollector.Configuration.ForTest) {
    var lastEventsAfter = EventId(-1)
    override def eventsAfter(after: EventId) = {
      lastEventsAfter = after
      super.eventsAfter(after)
    }
  }
}

object FatEventRouteTest
{
  private val TestWorkflowId = WorkflowPath("/test") % "VERSION"
  private val TestEvents = for (i ← 1 to 20) yield
    Stamped(EventId(i * 10), OrderId(i.toString) <-: OrderAdded(TestWorkflowId, None, Payload.empty))
  private val TestFatEvents =
    for (event ← TestEvents) yield
      Stamped(event.eventId, event.timestamp,
        event.value.key <-: OrderAddedFat(None, OrderAddedFat.Cause.UNKNOWN, TestWorkflowId, None, Map.empty))

  private def fatEventsAfter(after: EventId) = TestFatEvents dropWhile (_.eventId <= after)
}
