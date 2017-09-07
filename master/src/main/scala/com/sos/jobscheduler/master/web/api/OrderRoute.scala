package com.sos.jobscheduler.master.web.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.html.{HtmlDirectives, WebServiceContext}
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.SomeEventRequest
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import com.sos.jobscheduler.master.KeyedEventJsonFormats.keyedEventJsonFormat
import com.sos.jobscheduler.master.OrderClient
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext
import com.sos.jobscheduler.master.web.simplegui.OrdersHtmlPage._
import scala.concurrent.ExecutionContext
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
trait OrderRoute extends HtmlDirectives[WebServiceContext] {

  protected def orderClient: OrderClient
  protected implicit def executionContext: ExecutionContext
  protected def eventCollector: EventCollector
  protected def eventIdGenerator: EventIdGenerator
  protected implicit def webServiceContext: MasterWebServiceContext

  def orderRoute: Route =
    pathSingleSlash {
      parameter("return".?) {
        case Some("OrderOverview") | None ⇒
          completeTryHtml(orderClient.orderOverviews)
        case Some("Order") | None ⇒
          complete(orderClient.orders)
        case Some("OrderEvent") ⇒
          orderEvents
        case _ ⇒
          reject
      }
    } ~
    path(Segment) { orderIdString ⇒
      singleOrder(OrderId(orderIdString))
    } ~
    extractUnmatchedPath {
      case path: Uri.Path.Slash ⇒ singleOrder(OrderId(path.tail.toString))  // Slashes not escaped
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
