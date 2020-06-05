package js7.master.web.master.api.order

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.{Conflict, Created, NotFound, OK}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Route}
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils._
import js7.base.generic.Completed
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers._
import js7.data.order.{FreshOrder, OrderId}
import js7.master.OrderApi
import js7.master.data.events.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import js7.master.web.common.MasterRouteProvider
import js7.master.web.master.api.order.OrderRoute._
import io.circe.Json
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute extends MasterRouteProvider
{
  protected def orderApi: OrderApi.WithCommands

  private implicit def implicitScheduler: Scheduler = scheduler

  final lazy val orderRoute: Route =
    authorizedUser(ValidUserPermission) { _ =>
      post {
        pathEnd {
          entity(as[Json]) { json =>
            if (json.isArray)
              json.as[Seq[FreshOrder]] match {
                case Left(failure) => complete(failure.toProblem)
                case Right(orders) =>
                  completeTask(
                    orderApi.addOrders(orders)
                      .map[ToResponseMarshallable](_.map((_: Completed) => OK)))
              }
            else
              json.as[FreshOrder] match {
                case Left(failure) => complete(failure.toProblem)
                case Right(order) =>
                  extractUri { uri =>
                    onSuccess(orderApi.addOrder(order).runToFuture) {
                      case Left(problem) => complete(problem)
                      case Right(isNoDuplicate) =>
                        respondWithHeader(Location(uri.withPath(uri.path / order.id.string))) {
                          complete(
                            if (isNoDuplicate) Created
                            else Conflict -> Problem.pure(s"Order '${order.id.string}' has already been added"))
                        }
                    }
                  }
              }
          }
        }
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

  private def singleOrder(orderId: OrderId): Route =
    completeTask(
      orderApi.order(orderId).map(_.map {
        case Some(o) =>
          o: ToResponseMarshallable
        case None =>
          Problem.pure(s"Does not exist: $orderId"): ToResponseMarshallable
      }))
}

object OrderRoute {
  intelliJuseImport(keyedEventJsonCodec)

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
