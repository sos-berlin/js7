package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.collector.{EventCollector, EventDirectives}
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.OrderFatEvent
import com.sos.jobscheduler.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderStdoutWrittenFat}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderStdoutWritten, OrderTransferredToAgent}
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRouteTest._
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.execution.Scheduler
import org.scalatest.{Args, FreeSpec}
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class FatEventRouteTest extends FreeSpec with RouteTester with FatEventRoute {

  override protected def runTest(testName: String, args: Args) = {
    logger.debug("-" * 50)
    logger.debug(s"""Test "$testName"""")
    val status = super.runTest(testName, args)
    status.whenCompleted {
      case Success(true) ⇒
      case Success(false) ⇒ logger.warn(s"""Test "$testName" FAILED""")
      case Failure(t) ⇒ logger.warn(s"""Test "$testName" FAILED: $t""")
    }
    status
  }

  private implicit val timeout = 99.seconds
  private implicit val routeTestTimeout = RouteTestTimeout(timeout)
  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventWatch = new TestEventWatch
  protected implicit def scheduler = Scheduler.global

  TestEvents.flatten foreach eventWatch.addStamped

  private def route = pathSegments("fatEvent")(fatEventRoute)

  for (uri ← List(
    "/fatEvent?return=OrderFatEvent&timeout=60&after=0",
    "/fatEvent?after=0"))
  {
    s"$uri" in {
      assert(getFatEvents(uri) == TestFatEvents)
    }
  }

  "Fetch events with repeated GET requests" - {  // Similar to EventRouteTest
    "/fatEvent?limit=3&after=0 start" in {
      assert(getFatEvents("/fatEvent?limit=3&after=0") == fatEventsAfter(0).take(3))
      assert(eventWatch.lastEventsAfter == 0)
    }

    "/fatEvent?limit=3&after=30 continue" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=30")
      assert(stampeds == fatEventsAfter(30).take(3))
      assert(stampeds.head.eventId == 40)
      assert(stampeds.last.eventId == 60)
      assert(eventWatch.lastEventsAfter == 30)
    }

    "/fatEvent?limit=3&after=60 continue" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=60")
      assert(stampeds == fatEventsAfter(60).take(3))
      assert(stampeds.head.eventId == 70)
      assert(stampeds.last.eventId == 90)
      assert(eventWatch.lastEventsAfter == 60)
    }

    "/fatEvent?limit=1&after=70 rewind in last read chunk" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=70")
      assert(stampeds == fatEventsAfter(70).take(3))
      assert(stampeds.head.eventId ==  80)
      assert(stampeds.last.eventId == 100)
      assert(eventWatch.lastEventsAfter == 60)  // Rewound to lastRequested
    }

    "/fatEvent?limit=3&after=80 continue" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=80")
      assert(stampeds == fatEventsAfter(80).take(3))
      assert(stampeds.head.eventId ==  90)
      assert(stampeds.last.eventId == 110)
      assert(eventWatch.lastEventsAfter == 70)  // Rewound to lastRequested
    }

    "/fatEvent?limit=3&after=60 rewind to oldest" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=60")
      assert(stampeds == fatEventsAfter(60).take(3))
      assert(stampeds.head.eventId == 70)
      assert(stampeds.last.eventId == 90)
      assert(eventWatch.lastEventsAfter == eventWatch.tornEventId)  // Rewound to tornEventId == BeforeFirst (no snapshot taken)
      assert(eventWatch.lastEventsAfter == EventId.BeforeFirst)
    }

    "/fatEvent?limit=3&after=150 skip some events" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=150")
      assert(stampeds == fatEventsAfter(150).take(3))
      assert(stampeds.head.eventId == 160)
      assert(stampeds.last.eventId == 180)
      assert(eventWatch.lastEventsAfter == 90)  // Skipped until after 150
    }

    "/fatEvent?after=180 no more events" in {
      assert(getFatEventSeq("/fatEvent?after=180") == EventSeq.Empty(181))  // One OrderEvent processed, not yet resulting in an OrderFatEvent
    }

    "/fatEvent?after=180, intermediate event added" in {
      eventWatch.addStamped(Stamped(EventId(190), OrderId("10") <-: OrderDetachable))  // Does not yield an OrderFatEvent
      assert(getFatEventSeq("/fatEvent?after=180") == EventSeq.Empty(190))
    }

    "/fatEvent?after=180, intermediate event added, with timeout" in {
      val t = now
      assert(getFatEventSeq("/fatEvent?after=180&timeout=0.1") == EventSeq.Empty(190))
      assert(now - t >= 100.millis)
    }

    "/fatEvent?after=190, OrderFinished added" in {
      val t = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(Stamped(EventId(191), OrderId("10") <-: OrderStdoutWritten("1")))
      }
      assert(getFatEventSeq("/fatEvent?timeout=30&after=190") == EventSeq.NonEmpty(
        Stamped(191, OrderId("10") <-: OrderStdoutWrittenFat("1")):: Nil))
      assert(now - t >= 100.millis + EventDirectives.DefaultDelay, "(DefaultDelay)")
    }

    "/fatEvent?after=191, with delay" in {
      val t = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(Stamped(EventId(192), OrderId("10") <-: OrderStdoutWritten("2")))
      }
      assert(getFatEventSeq("/fatEvent?delay=0.1&timeout=30&after=191") == EventSeq.NonEmpty(
        Stamped(192, OrderId("10") <-: OrderStdoutWrittenFat("2")):: Nil))
      assert(now - t >= 100.millis)
    }

    "/fatEvent?after=192, with delay=0" in {
      val t = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(Stamped(EventId(193), OrderId("10") <-: OrderStdoutWritten("3")))
      }
      assert(getFatEventSeq("/fatEvent?delay=0&timeout=30&after=192") == EventSeq.NonEmpty(
        Stamped(193, OrderId("10") <-: OrderStdoutWrittenFat("3")):: Nil))
      assert(now - t >= EventDirectives.MinimumDelay)
    }

    "/fatEvent?after=193 no more events, with timeout" in {
      val t = now
      assert(getFatEventSeq("/fatEvent?after=193&timeout=0.1") == EventSeq.Empty(193))
      assert(now - t >= 100.millis)
    }

    "After truncated journal" in {
      eventWatch.tear(50)
      assert(getFatEventSeq("/fatEvent?after=49") == TearableEventSeq.Torn(50))
      assert(getFatEventSeq("/fatEvent?limit=3&after=50") == TearableEventSeq.Torn(50))  // Must rewind to torn EventId.FirstEventId
    }
  }

  private def getFatEvents(uri: String): Seq[Stamped[KeyedEvent[OrderFatEvent]]] =
    getFatEventSeq(uri) match {
      case EventSeq.NonEmpty(stampeds) ⇒
        assert(stampeds.nonEmpty)
        stampeds

      case x ⇒ fail(s"Unexpected response: $x")
    }

  private def getFatEventSeq(uri: String): TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]] =
    Get(uri) ~> Accept(`application/json`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      responseAs[TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]]]
    }

  protected final class TestEventWatch extends EventCollector(EventCollector.Configuration.ForTest) {
    var lastEventsAfter = EventId(-1)
    override def eventsAfter(after: EventId) = {
      lastEventsAfter = after
      super.eventsAfter(after)
    }
  }
}

object FatEventRouteTest
{
  private val logger = Logger(getClass)
  private val TestWorkflowId = WorkflowPath("/test") % "VERSION"
  private val TestEvents: Seq[Seq[Stamped[KeyedEvent[OrderEvent.OrderCoreEvent]]]] =
    (1 to 18).map(i ⇒
      Stamped(EventId(i * 10    ), OrderId(i.toString) <-: OrderAdded(TestWorkflowId, None, Payload.empty)) ::  // Yields OrderFatEvent
      Stamped(EventId(i * 10 + 1), OrderId(i.toString) <-: OrderTransferredToAgent(AgentPath("/AGENT") % "1")) :: Nil)  // No FatEvent
  private val TestFatEvents =
    for (events ← TestEvents; event = events.head) yield
      Stamped(event.eventId, event.timestamp,
        event.value.key <-: OrderAddedFat(TestWorkflowId, None, Map.empty))

  private def fatEventsAfter(after: EventId) = TestFatEvents dropWhile (_.eventId <= after)
}
