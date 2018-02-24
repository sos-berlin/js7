package com.sos.jobscheduler.master.web.master.api.order

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.order.{Order, OrderId, OrderOverview, OrdersOverview}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.master.OrderClient
import com.sos.jobscheduler.master.web.master.api.order.OrderRouteTest._
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class OrderRouteTest extends FreeSpec with ScalatestRouteTest with OrderRoute {

  protected implicit def executionContext = system.dispatcher
  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventIdGenerator = new EventIdGenerator
  protected val orderClient = new OrderClient {
    def executionContext = OrderRouteTest.this.executionContext
    def order(orderId: OrderId) = Future.successful(TestOrders.get(orderId))
    def orders = Future.successful(eventIdGenerator.stamp(TestOrders.values.toVector))
    def orderCount = Future.successful(TestOrders.values.size)
  }

  private def route: Route =
    pathSegments("api/order") {
      orderRoute
    }

  // OrdersOverview
  OrderUri in {
    Get(OrderUri) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[OrdersOverview] == OrdersOverview(orderCount = TestOrders.size))
    }
  }

  // Seq[OrderOverview]
  for (uri ← List(
      s"$OrderUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Stamped(_, _, orders) = responseAs[Stamped[Seq[OrderOverview]]]
        assert(orders == (TestOrders.values.toList map OrderOverview.fromOrder))
      }
    }
  }

  // Seq[Order]
  for (uri ← List(
       s"$OrderUri/?return=Order")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Stamped(_, _, orders) = responseAs[Stamped[Seq[Order[Order.State]]]]
        assert(orders == TestOrders.values.toList)
      }
    }
  }

  // Order
  for (uri ← List(
       s"$OrderUri/${TestOrders.values.head.id.string}",
       s"$OrderUri/${TestOrders.values.head.id.string.replace("/", "%2F")}")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[Order[Order.State]] == TestOrders.values.head)
      }
    }
  }
}

object OrderRouteTest {
  private val OrderUri = "/api/order"
  private val TestOrders: Map[OrderId, Order[Order.State]] = List(
    Order(OrderId("/PATH/1"), WorkflowPath("/test"), Order.StartNow),
    Order(OrderId("2"), WorkflowPath("/test") /: Position(2), Order.Finished))
    .toKeyedMap { _.id }
}
