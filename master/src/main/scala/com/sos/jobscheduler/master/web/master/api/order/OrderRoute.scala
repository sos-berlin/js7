package com.sos.jobscheduler.master.web.master.api.order

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Conflict, Created, NotFound, OK}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Route}
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.OrderApi
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.order.OrderRoute._
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
                case Left(failure) => complete(BadRequest -> Option(failure.getMessage).getOrElse(failure.toString))
                case Right(orders) =>
                  completeTask(
                    orderApi.addOrders(orders)
                      .map[ToResponseMarshallable](_.map((_: Completed) => OK)))
              }
            else
              json.as[FreshOrder] match {
                case Left(failure) => complete(BadRequest -> Option(failure.getMessage).getOrElse(failure.toString))
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
      orderApi.order(orderId).map {
        case Some(o) =>
          o: ToResponseMarshallable
        case None =>
          Problem.pure(s"Does not exist: $orderId"): ToResponseMarshallable
      })
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
