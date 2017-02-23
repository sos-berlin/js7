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
    path(Segment) { orderIdString ⇒
      val orderId = OrderId(orderIdString)
      complete {
        master.getOrder(orderId) map {
          case Some(o) ⇒
            o: ToResponseMarshallable
          case None ⇒
            BadRequest → s"No such $orderId": ToResponseMarshallable
        }
      }
    }
}
