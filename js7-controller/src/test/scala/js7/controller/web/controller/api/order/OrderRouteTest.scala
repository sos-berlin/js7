package js7.controller.web.controller.api.order

import org.apache.pekko.http.scaladsl.marshalling.ToEntityMarshaller
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{BadRequest, Conflict, Created, OK}
import org.apache.pekko.http.scaladsl.model.headers.{Accept, Location}
import org.apache.pekko.http.scaladsl.server.Route
import io.circe.syntax.*
import io.circe.{Encoder, Json}
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.*
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.http.PekkoHttpUtils.*
import js7.controller.OrderApi
import js7.controller.web.controller.api.order.OrderRouteTest.*
import js7.controller.web.controller.api.test.RouteTester
import js7.core.command.CommandMeta
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders}
import js7.data.order.{FreshOrder, Order, OrderId, OrdersOverview}
import js7.data.value.StringValue
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.IORuntime

/**
  * @author Joacim Zschimmer
  */
final class OrderRouteTest extends OurTestSuite, RouteTester, OrderRoute:

  protected def whenShuttingDown = Deferred.unsafe
  protected def actorSystem = system
  protected val orderApi = new OrderApi:
    def order(orderId: OrderId) = IO(Right(TestOrders.get(orderId)))
    def orders = IO(Right(TestOrders.values.toVector))
    def orderCount = IO(Right(TestOrders.values.size))

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]] =
    (command match {
      case AddOrder(order) => IO(Right(AddOrder.Response(ignoredBecauseDuplicate = order.id == DuplicateOrderId)))
      case AddOrders(_) => IO(Right(AddOrders.Response(1234L)))
      case _ => IO(fail())
    }).map(_.map(_.asInstanceOf[command.Response]))

  private def route: Route =
    pathSegments("controller/api/order"):
      orderRoute

  private given IORuntime = ioRuntime

  // OrdersOverview
  "/controller/api/order" in:
    Get("/controller/api/order") ~> Accept(`application/json`) ~> route ~> check:
      assert(responseAs[OrdersOverview] == OrdersOverview(count = TestOrders.size))

  // Order
  for (uri <- List(
       "/controller/api/order//PATH/ORDER-1",
       "/controller/api/order/%2FPATH%2FORDER-1"))
    s"$uri" in:
      Get(uri) ~> Accept(`application/json`) ~> route ~> check:
        assert(status == OK && responseAs[Order[Order.State]] == TestOrders.values.head)

  "POST invalid order" in:
    val order = FreshOrder.unchecked(OrderId("ORDER|🔷"), WorkflowPath("WORKFLOW"))
    Post(s"/controller/api/order", order) ~> route ~> check:
      assert(status == BadRequest)
      assert(response.utf8String.await(99.s) == "JSON DecodingFailure at : OrderId must not contain reserved characters: |\n")

  "POST new order" in:
    val order = FreshOrder(OrderId("ORDER-🔷"), WorkflowPath("WORKFLOW"),
      Map("KEY" -> StringValue("VALUE")),
      Some(Timestamp.parse("2017-03-07T12:00:00Z")))
    Post(s"/controller/api/order", order) ~> Accept(`application/json`) ~> route ~> check:
      assert(status == Created)  // New order
      assert(response.header[Location] contains Location("http://example.com/controller/api/order/ORDER-%F0%9F%94%B7"))
      assert(responseAs[Json] == Json.obj())

  "POST duplicate order" in:
    val order = FreshOrder(DuplicateOrderId, WorkflowPath("WORKFLOW"))
    Post("/controller/api/order", order) ~> Accept(`application/json`) ~> route ~> check:
      assert(status == Conflict)  // Duplicate order
      assert(response.header[Location] contains Location(s"http://example.com/controller/api/order/DUPLICATE"))
      assert(responseAs[Json] == Problem("Order:DUPLICATE has already been added").asJson(Problem.typedJsonEncoder))

  "POST multiple orders" in:
    val orders = FreshOrder(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW")) :: FreshOrder(DuplicateOrderId, WorkflowPath("WORKFLOW")) :: Nil
    implicit val toEntityMarshaller: ToEntityMarshaller[Seq[FreshOrder]] =
      jsonMarshaller(implicitly[Encoder[Seq[FreshOrder]]])
    Post("/controller/api/order", orders) ~> Accept(`application/json`) ~> route ~> check:
      assert(status == OK)
      assert(response.header[Location].isEmpty)
      assert(responseAs[ControllerCommand.Response] == AddOrders.Response(1234L))


object OrderRouteTest:
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val TestOrders: Map[OrderId, Order[Order.State]] = List(
    Order(OrderId("/PATH/ORDER-1"), TestWorkflowId /: Position(0), Order.Fresh),
    Order(OrderId("ORDER-2"), TestWorkflowId /: Position(2), Order.Finished)
  ).toKeyedMap(_.id)
  private val DuplicateOrderId = OrderId("DUPLICATE")
