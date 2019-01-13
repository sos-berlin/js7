package com.sos.jobscheduler.master.web.master.api.order

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.{Conflict, Created, NotFound}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Route}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.OrderApi
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.order.OrderRoute._
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute extends MasterRouteProvider
{
  protected def orderApi: OrderApi.WithCommands

  private implicit def implicitScheduler: Scheduler = scheduler

  final lazy val orderRoute: Route =
    authorizedUser(ValidUserPermission) { _ ⇒
      post {
        pathEnd {
          entity(as[FreshOrder]) { order ⇒
            extractUri { uri ⇒
              respondWithHeader(Location(uri + "/" + order.id.string)) {
                completeTask(
                    orderApi.addOrder(order).map[ToResponseMarshallable] {
                    case Invalid(problem) ⇒ problem
                    case Valid(false) ⇒ Conflict → Problem(s"Order '${order.id.string}' has already been added")
                    case Valid(true) ⇒ Created
                  })
              }
            }
          }
        }
      } ~
      get {
        pathEnd {
          parameter("return".?) {
            case None ⇒
              complete(orderApi.ordersOverview.runToFuture)
            case _ ⇒
              complete(Problem("Parameter return is not supported here"))
          }
        } ~
        pathSingleSlash {
          parameter("return".?) {
            case None ⇒
              complete(orderApi.orderIds.runToFuture)
            case Some("Order") ⇒
              complete(orderApi.orders.runToFuture)
            case Some(unrecognized) ⇒
              complete(Problem(s"Unrecognized return=$unrecognized"))
          }
        } ~
        matchOrderId { orderId ⇒
          singleOrder(orderId)
        }
      }
    }

  private def singleOrder(orderId: OrderId): Route =
    completeTask(
      orderApi.order(orderId).map {
        case Some(o) ⇒
          o: ToResponseMarshallable
        case None ⇒
          Problem(s"Does not exist: $orderId"): ToResponseMarshallable
      })

}

object OrderRoute {
  intelliJuseImport(keyedEventJsonCodec)

  private val matchOrderId = new Directive[Tuple1[OrderId]] {
    def tapply(inner: Tuple1[OrderId] ⇒ Route) =
      path(Segment) { orderIdString ⇒
        inner(Tuple1(OrderId(orderIdString)))
      } ~
      extractUnmatchedPath {
        case Uri.Path.Slash(tail) if !tail.isEmpty ⇒
          inner(Tuple1(OrderId(tail.toString)))  // Slashes not escaped
        case _ ⇒
          complete(NotFound)  // Invalid OrderId syntax
      }
  }
}
