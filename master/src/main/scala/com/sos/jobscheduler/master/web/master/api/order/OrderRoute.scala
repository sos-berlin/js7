package com.sos.jobscheduler.master.web.master.api.order

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.{Conflict, Created, NotFound}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import com.sos.jobscheduler.master.OrderClient
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute {

  protected def orderClient: OrderClient
  protected implicit def executionContext: ExecutionContext

  def orderRoute: Route =
    post {
      pathEnd {
        entity(as[FreshOrder]) { order ⇒
          extractUri { uri ⇒
            respondWithHeader(Location(uri + "/" + order.id.string)) {
              complete {
                orderClient.addOrder(order) map {
                  case true ⇒ Created: ToResponseMarshallable
                  case false ⇒ Conflict → Problem(s"Order '${order.id.string}' has already been added"): ToResponseMarshallable
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
          case None ⇒
            complete(orderClient.ordersOverview)
          case _ ⇒
            complete(Problem("Parameter return is not supported here"))
        }
      } ~
      pathSingleSlash {
        parameter("return".?) {
          case Some("OrderOverview") | None ⇒
            complete(orderClient.orderOverviews)
          case Some("Order") | None ⇒
            complete(orderClient.orders)
          case Some(unrecognized) ⇒
            complete(Problem(s"Unknown return=$unrecognized"))
        }
      } ~
      path(Segment) { orderIdString ⇒
        singleOrder(OrderId(orderIdString))
      } ~
      extractUnmatchedPath {
        case Uri.Path.Slash(tail) if !tail.isEmpty ⇒
          singleOrder(OrderId(tail.toString))  // Slashes not escaped
        case _ ⇒
          complete(NotFound)
      }
    }

  private def singleOrder(orderId: OrderId): Route =
    complete {
      orderClient.order(orderId) map {
        case Some(o) ⇒
          o: ToResponseMarshallable
        case None ⇒
          Problem(s"Does not exist: $orderId"): ToResponseMarshallable
      }
    }
}

object OrderRoute {
  intelliJuseImport(keyedEventJsonCodec)
}
