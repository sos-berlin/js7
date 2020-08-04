package js7.controller.web.controller.api.graphql

import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/html`, `text/plain`}
import akka.http.scaladsl.model.StatusCodes.NotAcceptable
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.util.ByteString
import io.circe.Json
import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.implicits.RichTraversable
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.http.CirceJsonSupport._
import js7.common.scalautil.Futures.implicits._
import js7.controller.OrderApi
import js7.controller.web.controller.api.graphql.GraphqlRouteTest._
import js7.controller.web.controller.api.test.RouteTester
import js7.core.item.InventoryItemApi
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GraphqlRouteTest extends AnyFreeSpec with RouteTester with GraphqlRoute {

  protected def whenShuttingDown = Future.never
  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  protected implicit def scheduler: Scheduler = Scheduler.Implicits.global
  protected val itemApi = InventoryItemApi.forTest(Map.empty)
  protected val orderApi = new OrderApi {
    def order(orderId: OrderId) = Task(Right(TestOrders.get(orderId)))
    def orders = Task(Right(TestOrders.values.toVector))
    def orderCount = Task(Right(TestOrders.values.size))
  }

  private def route = Route.seal {
    pathSegments("controller/api/graphql") {
      graphqlRoute
    }
  }

  "/controller/api/graphql/schema" in {
    Get("/controller/api/graphql/schema") ~> Accept(`text/plain`) ~> route ~> check {
      implicit val u = Unmarshaller.byteStringUnmarshaller.forContentTypes(`text/plain`)
      assert(responseAs[ByteString].utf8String contains """
        |type Order {
        |  id: OrderId!
        |
        |  "A child Order has a parent Order"
        |  parent: OrderId
        |
        |  "The Order's current WorkflowId and Position in this Workflow"
        |  workflowPosition: WorkflowPosition!
        |  workflowPath: WorkflowPath!
        |
        |  "Order is attaching to, attached to, or detaching from an Agent"
        |  attachedState: Order_AttachedState
        |  lastOutcome: Outcome!
        |  state: OrderState!
        |  arguments: StringMap
        |  scheduledFor: Long
        |  childOrderIds: [OrderId!]
        |  offeredOrderId: OrderId
        |  problem: Problem
        |}
        |""".stripMargin)
    }
    Get("/controller/api/graphql/schema") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == NotAcceptable)
      assert(response.utf8StringFuture.await(99.s) contains "text/plain")
    }
  }

  "/controller/api/graphql" - {
    "text/html - GraphiQL" in {
      Get("/controller/api/graphql") ~> Accept(`text/html`) ~> route ~> check {
        implicit val u = Unmarshaller.byteStringUnmarshaller.forContentTypes(`text/html`)
        assert(responseAs[ByteString].utf8String contains "<title>JS7 Controller · GraphQL</title>")
      }
    }

    "query" in {
      Get("""/controller/api/graphql?query={order(id:"1"){id,workflowPath}}""") ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[Json] ==
          json"""{
            "data": {
              "order": {
                "id": "1",
                "workflowPath": "/A-WORKFLOW"
              }
            }
          }""")
      }
    }
  }

  // More tests in ControllerWebServiceTest
}

object GraphqlRouteTest {
  private val TestOrders: Map[OrderId, Order[Order.State]] =
    Vector(
      Order(OrderId("1"), (WorkflowPath("/A-WORKFLOW") ~ "1") /: Position(0), Order.Fresh(None))
    ).toKeyedMap(_.id)
}
