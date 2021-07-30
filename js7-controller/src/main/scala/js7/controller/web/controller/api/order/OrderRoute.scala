package js7.controller.web.controller.api.order

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.{Conflict, Created, NotFound, UnsupportedMediaType}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Route}
import cats.syntax.flatMap._
import io.circe.Json
import js7.base.auth.{SimpleUser, ValidUserPermission}
import js7.base.circeutils.CirceUtils._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ByteSequenceToLinesObservable
import js7.base.utils.ScalaUtils.syntax.RichAny
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport._
import js7.controller.OrderApi
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.order.OrderRoute._
import js7.core.command.CommandMeta
import js7.core.web.EntitySizeLimitProvider
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders, DeleteOrdersWhenTerminated}
import js7.data.order.{FreshOrder, OrderId}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute
extends ControllerRouteProvider with EntitySizeLimitProvider
{
  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]]
  protected def orderApi: OrderApi
  protected def actorSystem: ActorSystem

  private implicit def implicitScheduler: Scheduler = scheduler
  private implicit def implicitActorsystem = actorSystem

  final lazy val orderRoute: Route =
    authorizedUser(ValidUserPermission) { user =>
      post {
        pathEnd {
          withSizeLimit(entitySizeLimit)(
            entity(as[HttpEntity])(httpEntity =>
              if (httpEntity.contentType == `application/x-ndjson`.toContentType)
                completeTask {
                  val startedAt = now
                  var byteCount = 0L
                  httpEntity
                    .dataBytes
                    .toObservable
                    .map(_.toByteArray)
                    .pipeIf(logger.underlying.isDebugEnabled)(_.map { o => byteCount += o.length; o })
                    .flatMap(new ByteSequenceToLinesObservable)
                    .mapParallelOrderedBatch()(_
                      .parseJsonAs[FreshOrder].orThrow)
                    .toL(Vector)
                    .flatTap(orders => Task {
                      val d = startedAt.elapsed
                      if (d > 1.s) logger.debug(s"post controller/api/order received - " +
                        itemsPerSecondString(d, orders.size, "orders") + " · " +
                        bytesPerSecondString(d, byteCount))
                    })
                    .flatMap(orders => executeCommand(AddOrders(orders), CommandMeta(user)))
                    .map(_.map(o => o: ControllerCommand.Response))
                }
              else
                entity(as[Json]) { json =>
                  if (json.isArray)
                    json.as[Seq[FreshOrder]] match {
                      case Left(failure) => complete(failure.toProblem)
                      case Right(orders) =>
                        completeTask(
                          executeCommand(AddOrders(orders), CommandMeta(user))
                            .map(_.map(o => o: ControllerCommand.Response)))
                    }
                  else
                    json.as[FreshOrder] match {
                      case Left(failure) => complete(failure.toProblem)
                      case Right(order) =>
                        extractUri { uri =>
                          onSuccess(executeCommand(AddOrder(order), CommandMeta(user)).runToFuture) {
                              case Left(problem) => complete(problem)
                              case Right(response) =>
                                respondWithHeader(Location(uri.withPath(uri.path / order.id.string))) {
                                  complete(
                                    if (response.ignoredBecauseDuplicate)
                                      Conflict -> Problem.pure(s"Order '${order.id.string}' has already been added")
                                    else
                                      Created -> emptyJsonObject)
                                }
                            }
                        }
                    }
                }))
        } ~
          pathPrefix("DeleteOrdersWhenTerminated")(
            pathEnd(
              deleteOrdersWhenTerminated(user)))
      } ~
      get {
        pathEnd {
          parameter("return".?) {
            case None =>
              complete(orderApi.ordersOverview.runToFuture)  // TODO Should be streamed
            case _ =>
              complete(Problem.pure("Parameter return is not supported here"))
          }
        } ~
        pathSingleSlash {
          parameter("return".?) {
            case None =>
              complete(orderApi.orderIds.runToFuture)  // TODO Should be streamed
            case Some("Order") =>
              complete(orderApi.orders.runToFuture)   // TODO Should be streamed
            case Some(unrecognized) =>
              complete(Problem.pure(s"Unrecognized return=$unrecognized"))
          }
        } ~
        matchOrderId { orderId =>
          singleOrder(orderId)
        }
      }
    }

  private def deleteOrdersWhenTerminated(user: SimpleUser): Route =
    withSizeLimit(entitySizeLimit)(
      entity(as[HttpEntity])(httpEntity =>
        if (httpEntity.contentType != `application/x-ndjson`.toContentType)
          complete(UnsupportedMediaType)
        else
          completeTask(
            httpEntity
              .dataBytes
              .toObservable
              .map(_.toByteArray)
              .flatMap(new ByteSequenceToLinesObservable)
              .mapParallelOrderedBatch()(_
                .parseJsonAs[OrderId].orThrow)
              .toL(Vector)
              .map(DeleteOrdersWhenTerminated(_))
              .flatMap(executeCommand(_, CommandMeta(user)))
              .map(_.map(o => o: ControllerCommand.Response)))))

  private def singleOrder(orderId: OrderId): Route =
    completeTask(
      orderApi.order(orderId).map(_.map {
        case Some(o) =>
          o: ToResponseMarshallable
        case None =>
          Problem.pure(s"Does not exist: $orderId"): ToResponseMarshallable
      }))
}

object OrderRoute
{
  private val emptyJsonObject = Json.obj()
  private val logger = Logger(getClass)

  private val matchOrderId = new Directive[Tuple1[OrderId]] {
    def tapply(inner: Tuple1[OrderId] => Route) =
      path(Segment) { orderIdString =>
        inner(Tuple1(OrderId(orderIdString)))
      } ~
      extractUnmatchedPath {
        case Uri.Path.Slash(tail) if !tail.isEmpty =>
          inner(Tuple1(OrderId(tail.toString)))  // Slashes not escaped
        case _ =>
          complete(NotFound)  // Invalid OrderId syntax
      }
  }
}
