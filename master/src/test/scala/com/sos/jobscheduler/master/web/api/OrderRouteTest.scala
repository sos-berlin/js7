package com.sos.jobscheduler.master.web.api

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, OrderOverview}
import com.sos.jobscheduler.master.OrderClient
import com.sos.jobscheduler.master.web.api.OrderRouteTest._
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.Future
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.routing.Route
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class OrderRouteTest extends FreeSpec with ScalatestRouteTest with OrderRoute {

  protected implicit def executionContext = system.dispatcher
  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventCollector = new EventCollector.ForTest
  protected val eventIdGenerator = new EventIdGenerator
  protected val orderClient = new OrderClient {
    def executionContext = OrderRouteTest.this.executionContext
    def order(orderId: OrderId) = Future.successful(TestOrders.get(orderId))
    def orders = Future.successful(eventIdGenerator.stamp(TestOrders.values.toVector))
  }
  protected val webServiceContext = new MasterWebServiceContext

  TestEvents foreach eventCollector.addStamped

  private def route: Route =
    pathSegments("api/order") {
      orderRoute
    }

  OrderUri in {
    Get(OrderUri) ~> Accept(`application/json`) ~> route ~> check {
      assert(!handled)
    }
  }

  for (uri ← List(
      s"$OrderUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Stamped(_, orders) = responseAs[Stamped[Seq[OrderOverview]]]
        assert(orders == (TestOrders.values.toList map OrderOverview.fromOrder))
      }
    }
  }

  for (uri ← List(
      s"$OrderUri/?return=Order")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Stamped(_, orders) = responseAs[Stamped[Seq[Order[Order.State]]]]
        assert(orders == TestOrders.values.toList)
      }
    }
  }

  for (uri ← List(
      s"$OrderUri/?return=OrderEvent&timeout=60s&after=0")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Stamped(_, EventSeq.NonEmpty(stampeds)) = responseAs[Stamped[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]]
        assert(stampeds == TestEvents)
      }
    }
  }
}

object OrderRouteTest {
  private val OrderUri = "/api/order"
  private val TestOrders: Map[OrderId, Order[Order.State]] = List(
    Order(OrderId("1"), NodeKey(JobnetPath("/test"), NodeId("100")), Order.StartNow),
    Order(OrderId("2"), NodeKey(JobnetPath("/test"), NodeId("200")), Order.Finished))
    .toKeyedMap { _.id }
  private val TestEvents = List(
    Stamped(EventId(111222),
      KeyedEvent(
        OrderAdded(NodeKey(JobnetPath("/test"), NodeId("100")), Order.StartNow, Map(), Order.InitialOutcome))
        (OrderId("1"))))
}
