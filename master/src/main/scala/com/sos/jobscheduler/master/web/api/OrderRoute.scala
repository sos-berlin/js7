package com.sos.jobscheduler.master.web.api

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.Master
import scala.concurrent.ExecutionContext
import spray.http.StatusCodes.BadRequest
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.ToResponseMarshallable
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute {

  protected def master: Master
  protected implicit def executionContext: ExecutionContext

  def orderRoute: Route =
    //pathSingleSlash {
    //  complete {
    //    ...
    //  }
    //} ~
    path(Segment) { orderIdString ⇒
      singleOrder(OrderId(orderIdString))
    }

  private def singleOrder(orderId: OrderId): Route =
      complete {
        master.order(orderId) map {
          case Some(o) ⇒
            o: ToResponseMarshallable
          case None ⇒
            BadRequest → s"Does not exist: $orderId": ToResponseMarshallable
        }
      }
}
