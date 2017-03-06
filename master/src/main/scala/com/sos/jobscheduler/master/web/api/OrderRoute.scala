package com.sos.jobscheduler.master.web.api

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.data.event.SomeEventRequest
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import com.sos.jobscheduler.master.KeyedEventJsonFormats.keyedEventJsonFormat
import com.sos.jobscheduler.master.OrderClient
import scala.concurrent.ExecutionContext
import spray.http.StatusCodes.BadRequest
import spray.http.Uri
import spray.httpx.marshalling.ToResponseMarshallable
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute {

  protected def orderClient: OrderClient
  protected implicit def executionContext: ExecutionContext
  protected def eventCollector: EventCollector
  protected def eventIdGenerator: EventIdGenerator

  def orderRoute: Route =
    pathSingleSlash {
      parameter("return".?) {
        case Some("Order") | None ⇒
          complete(orderClient.orders)
        case Some("OrderEvent") ⇒
          orderEvents
        case _ ⇒
          reject
      }
    } ~
    unmatchedPath {
      case path: Uri.Path.Slash ⇒ singleOrder(OrderId(path.tail.toString))
      case _ ⇒ reject
    }

  private def singleOrder(orderId: OrderId): Route =
    complete {
      orderClient.order(orderId) map {
        case Some(o) ⇒
          o: ToResponseMarshallable
        case None ⇒
          BadRequest → s"Does not exist: $orderId": ToResponseMarshallable
      }
    }

  private def orderEvents: Route =
    eventRequest[OrderEvent]().apply {
      case request: SomeEventRequest[OrderEvent] ⇒
        complete {
          eventIdGenerator.stampTearableEventSeq {
            eventCollector.byPredicate[OrderEvent](request, predicate = _ ⇒ true)
          }
        }
      case _ ⇒
        reject
    }
}
