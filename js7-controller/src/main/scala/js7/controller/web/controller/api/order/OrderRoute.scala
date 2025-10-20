package js7.controller.web.controller.api.order

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import io.circe.Json
import js7.base.auth.{SimpleUser, ValidUserPermission}
import js7.base.circeutils.CirceUtils.*
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.LineSplitterPipe
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
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
  extends ControllerRouteProvider, EntitySizeLimitProvider:
  protected def executeCommand(command: ControllerCommand, meta: CommandMeta)
  : IO[Checked[command.Response]]
  protected def orderApi: OrderApi
  protected def actorSystem: ActorSystem

  private given IORuntime = ioRuntime
  private given ActorSystem = actorSystem

  final lazy val orderRoute: Route =
    authorizedUser(ValidUserPermission) { user =>
      post {
        pathEnd:
          withSizeLimit(entitySizeLimit):
            entity(as[HttpEntity]): httpEntity =>
              if httpEntity.contentType == `application/x-ndjson`.toContentType then
                addOrdersNdjson(httpEntity, user)
              else
                entity(as[Json]): json =>
                  if json.isArray then
                    addOrdersJsonArray(json, user)
                  else
                    json.as[FreshOrder] match
                      case Left(failure) => complete(failure.toProblem)
                      case Right(order) => addSingleOrder(order, user)
        ~
          pathPrefix("DeleteOrdersWhenTerminated"):
            pathEnd:
              deleteOrdersWhenTerminated(user)
      } ~
        get:
          pathEnd:
            parameter("return".?):
              case None =>
                complete(orderApi.ordersOverview.unsafeToFuture()) // TODO Should be streamed
              case _ =>
                complete(Problem.pure("Parameter return is not supported here"))
          ~
            matchOrderId: orderId =>
              singleOrder(orderId)
    }

  private def addOrdersNdjson(httpEntity: HttpEntity, user: SimpleUser) =
    completeIO:
      val startedAt = now
      var byteCount = 0L
      httpEntity.dataBytes
        .asFs2Stream(bufferSize = prefetch)
        //.chunks.evalTap(chunk => IO(logger.trace(s"### chunk.size=${chunk.size}"))).unchunks
        .pipeIf(logger.underlying.isDebugEnabled):
          _.map { o => byteCount += o.length; o }
        .through:
          LineSplitterPipe()
        .mapParallelBatch(prefetch = prefetch):
          _.parseJsonAs[FreshOrder]
        .compile
        .toVector
        .map(_.sequence)
        .flatTap: checkedOrders =>
          IO:
            for orders <- checkedOrders do
              val d = startedAt.elapsed
              if d > 1.s then logger.debug("post controller/api/order received - " +
                itemsPerSecondString(d, orders.size, "orders") + " · " +
                bytesPerSecondString(d, byteCount))
        .flatMapT: orders =>
          executeCommand(AddOrders(orders), CommandMeta(user))
        .map(_.map(o => o: ControllerCommand.Response))

  private def addOrdersJsonArray(json: Json, user: SimpleUser) =
    json.as[Vector[FreshOrder]] match
      case Left(failure) => complete(failure.toProblem)
      case Right(orders) =>
        completeIO:
          executeCommand(AddOrders(orders), CommandMeta(user))
            .map(_.map(o => o: ControllerCommand.Response))

  private def addSingleOrder(order: FreshOrder, user: SimpleUser) =
    extractUri: uri =>
      onSuccess(executeCommand(AddOrder(order), CommandMeta(user)).unsafeToFuture()):
        case Left(problem) => complete(problem)
        case Right(response) =>
          respondWithHeader(Location(uri.withPath(uri.path / order.id.string))):
            complete:
              if response.ignoredBecauseDuplicate then
                Conflict -> Problem.pure(s"${order.id} has already been added")
              else
                Created -> emptyJsonObject

  private def deleteOrdersWhenTerminated(user: SimpleUser): Route =
    withSizeLimit(entitySizeLimit):
      entity(as[HttpEntity]): httpEntity =>
        if httpEntity.contentType != `application/x-ndjson`.toContentType then
          complete(UnsupportedMediaType)
        else
          completeIO:
            httpEntity.dataBytes
              .asFs2Stream(bufferSize = prefetch)
              .through(LineSplitterPipe())
              .mapParallelBatch():
                _.parseJsonAs[OrderId].orThrow
              .compile
              .toVector
              .map:
                DeleteOrdersWhenTerminated(_)
              .flatMap:
                executeCommand(_, CommandMeta(user))
              .mapmap(o => o: ControllerCommand.Response)

  private def singleOrder(orderId: OrderId): Route =
    completeIO:
      orderApi.order(orderId).mapmap:
        case Some(o) =>
          o: ToResponseMarshallable
        case None =>
          Problem.pure(s"Does not exist: $orderId"): ToResponseMarshallable


object OrderRoute:
  private val emptyJsonObject = Json.obj()
  private val logger = Logger[this.type]

  private val matchOrderId = new Directive[Tuple1[OrderId]]:
    def tapply(inner: Tuple1[OrderId] => Route) =
      path(Segment): orderIdString =>
        inner(Tuple1(OrderId(orderIdString)))
      ~
        extractUnmatchedPath:
          case Uri.Path.Slash(tail) if !tail.isEmpty =>
            inner(Tuple1(OrderId(tail.toString))) // Slashes not escaped
          case _ =>
            complete(NotFound) // Invalid OrderId syntax
