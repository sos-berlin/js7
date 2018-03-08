package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.web.master.api.EventRouteTest._
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends FreeSpec with ScalatestRouteTest with EventRoute {

  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventCollector = new EventCollector.ForTest
  protected val eventIdGenerator = new EventIdGenerator
  protected implicit def executionContext = system.dispatcher

  TestEvents foreach eventCollector.addStamped

  private def route: Route =
    pathSegments("event") {
      eventRoute
    }

  for (uri ← List(
    s"/event?return=OrderEvent&timeout=60&after=0",
    s"/event?timeout=60&after=0")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        if (status != OK) fail(s"$status - ${responseEntity.toStrict(9.seconds).value}")
        val Stamped(_, _, EventSeq.NonEmpty(stampeds)) = responseAs[Stamped[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]]
        assert(stampeds == TestEvents)
      }
    }
  }
}

object EventRouteTest {
  intelliJuseImport(jsonUnmarshaller)

  private val TestEvents = List(
    Stamped(EventId(111222), Timestamp.ofEpochMilli(1000),
      OrderId("1") <-: OrderAdded(WorkflowPath("/test") % "VERSION", None, Payload.empty)))
}
