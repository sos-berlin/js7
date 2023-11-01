package js7.controller.web.controller.api.order

import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import io.circe.Json
import js7.base.auth.{SimpleUser, ValidUserPermission}
import js7.base.circeutils.CirceUtils.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ByteSequenceToLinesObservable
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEitherF}
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeTask
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.controller.OrderApi
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.order.OrderRoute.*
import js7.core.command.CommandMeta
import js7.core.web.EntitySizeLimitProvider
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders, DeleteOrdersWhenTerminated}
import js7.data.order.{FreshOrder, OrderId}
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Conflict, Created, NotFound, UnsupportedMediaType}
import org.apache.pekko.http.scaladsl.model.headers.Location
import org.apache.pekko.http.scaladsl.model.{HttpEntity, Uri}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive, Route}
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
  private implicit def implicitActorsystem: ActorSystem = actorSystem

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
                    .pipeIf(logger.underlying.isDebugEnabled)(_.map { o => byteCount += o.length; o })
                    .flatMap(new ByteSequenceToLinesObservable)
                    .mapParallelBatch()(_
                      .parseJsonAs[FreshOrder])
                    .toL(Vector)
                    .map(_.sequence)
                    .flatTap(checkedOrders => Task(
                      for (orders <- checkedOrders) {
                        val d = startedAt.elapsed
                        if (d > 1.s) logger.debug("post controller/api/order received - " +
                          itemsPerSecondString(d, orders.size, "orders") + " · " +
                          bytesPerSecondString(d, byteCount))
                       }))
                    .flatMapT(orders => executeCommand(AddOrders(orders), CommandMeta(user)))
                    .map(_.map(o => o: ControllerCommand.Response))
                }
              else
                entity(as[Json]) { json =>
                  if (json.isArray)
                    json.as[Vector[FreshOrder]] match {
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
                                    Conflict -> Problem.pure(s"${order.id} has already been added")
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
              .flatMap(new ByteSequenceToLinesObservable)
              .mapParallelBatch()(_
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
