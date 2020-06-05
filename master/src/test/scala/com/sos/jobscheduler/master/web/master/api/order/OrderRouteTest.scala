package com.sos.jobscheduler.master.web.master.api.order

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Conflict, Created, OK}
import akka.http.scaladsl.model.headers.{Accept, Location}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.AkkaHttpUtils._
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId, OrdersOverview}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.OrderApi
import com.sos.jobscheduler.master.web.master.api.order.OrderRouteTest._
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderRouteTest extends AnyFreeSpec with RouteTester with OrderRoute
{
  protected def isShuttingDown = false
  protected implicit def scheduler: Scheduler = Scheduler.global
  protected val orderApi = new OrderApi.WithCommands {
    def addOrder(order: FreshOrder) = Task(Right(order.id != DuplicateOrderId))
    def addOrders(orders: Seq[FreshOrder]) = Task(Right(Completed))
    def order(orderId: OrderId) = Task(Right(TestOrders.get(orderId)))
    def orders = Task(Right(TestOrders.values.toVector))
    def orderCount = Task(Right(TestOrders.values.size))
  }

  private def route: Route =
    pathSegments("master/api/order") {
      orderRoute
    }

  // OrdersOverview
  "/master/api/order" in {
    Get("/master/api/order") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[OrdersOverview] == OrdersOverview(count = TestOrders.size))
    }
  }

  // Seq[OrderId]
  for (uri <- List("/master/api/order/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Right(orders) = responseAs[Checked[Seq[OrderId]]]
        assert(status == OK && orders == TestOrders.values.map(_.id).toList)
      }
    }
  }

  // Seq[Order]
  for (uri <- List("/master/api/order/?return=Order")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        val Right(orders) = responseAs[Checked[Seq[Order[Order.State]]]]
        assert(status == OK && orders == TestOrders.values.toList)
      }
    }
  }

  // Order
  for (uri <- List(
       "/master/api/order//PATH/ORDER-1",
       "/master/api/order/%2FPATH%2FORDER-1")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK && responseAs[Order[Order.State]] == TestOrders.values.head)
      }
    }
  }

  "POST invalid order" in {
    val order = FreshOrder.unchecked(OrderId("ORDER/🔵"), WorkflowPath("/WORKFLOW"))
    Post(s"/master/api/order", order) ~> route ~> check {
      assert(status == BadRequest)  // New order
      assert(response.utf8StringFuture.await(99.s) == "JSON DecodingFailure at : OrderId must not contain reserved characters /\n")
    }
  }

  "POST new order" in {
    val order = FreshOrder(OrderId("ORDER-🔵"), WorkflowPath("/WORKFLOW"), Some(Timestamp.parse("2017-03-07T12:00:00Z")), Map("KEY" -> "VALUE"))
    Post(s"/master/api/order", order) ~> route ~> check {
      assert(status == Created)  // New order
      assert(response.header[Location] contains Location("http://example.com/master/api/order/ORDER-%F0%9F%94%B5"))
    }
  }

  "POST duplicate order" in {
    val order = FreshOrder(DuplicateOrderId, WorkflowPath("/WORKFLOW"))
    Post("/master/api/order", order) ~> route ~> check {
      assert(status == Conflict)  // Duplicate order
      assert(response.header[Location] contains Location(s"http://example.com/master/api/order/DUPLICATE"))
    }
  }

  "POST multiple orders" in {
    val orders = FreshOrder(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW")) :: FreshOrder(DuplicateOrderId, WorkflowPath("/WORKFLOW")) :: Nil
    Post("/master/api/order", orders) ~> route ~> check {
      assert(status == OK)
      assert(response.header[Location].isEmpty)
    }
  }
}

object OrderRouteTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  private val TestOrders: Map[OrderId, Order[Order.State]] = List(
    Order(OrderId("/PATH/ORDER-1"), TestWorkflowId, Order.Fresh.StartImmediately),
    Order(OrderId("ORDER-2"), TestWorkflowId /: Position(2), Order.Finished)
  ).toKeyedMap { _.id }
  private val DuplicateOrderId = OrderId("DUPLICATE")
}
