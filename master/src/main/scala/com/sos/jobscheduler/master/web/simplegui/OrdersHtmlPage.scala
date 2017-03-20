package com.sos.jobscheduler.master.web.simplegui

import com.sos.jobscheduler.common.sprayutils.html.HtmlDirectives.ToHtmlPage
import com.sos.jobscheduler.data.event.{Event, Stamped}
import com.sos.jobscheduler.data.order.{Order, OrderOverview}
import com.sos.jobscheduler.master.web.simplegui.OrdersHtmlPage._
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import scalatags.Text.all._
import spray.http.Uri

/**
  * @author Joacim Zschimmer
  */
final class OrdersHtmlPage private(
  stampedOrderOverviews: Stamped[Seq[OrderOverview]],
  protected val pageUri: Uri,
  protected val webServiceContext: MasterWebServiceContext)
extends MasterHtmlPage {

  protected def eventId = stampedOrderOverviews.eventId
  private val orderOverviews = stampedOrderOverviews.value

  def wholePage =
    htmlPage(
      div(cls := "ContentBox")(
        table(cls := "table table-condensed table-hover")(
          thead(
            tr(
              th("OrderId"),
              th("Jobnet"),
              th("Node"),
              th("State"))),
          tbody(
            for (order ← (orderOverviews sortBy { _.id }).reverse) yield {
              val rowCssClass = orderToTrClass(order) getOrElse  ""
              tr(cls := s"$rowCssClass clickable", data("href") := webServiceContext.uriString(Uri(path = Uri.Path("api/order") / order.id.string).toString))(
                td(order.id.string),
                td(order.nodeKey.jobnetPath.string),
                td(order.nodeKey.nodeId.string),
                td(order.state.toString))
            }
          ))))
}

object OrdersHtmlPage {
  implicit def orderOverviewsToHtmlPage[E <: Event](implicit webServiceContext: MasterWebServiceContext, ec: ExecutionContext): ToHtmlPage[Stamped[Seq[OrderOverview]]] =
    ToHtmlPage[Stamped[Seq[OrderOverview]]] { (stamped, pageUri) ⇒
      Future.successful(new OrdersHtmlPage(stamped, pageUri, webServiceContext))
    }

  def toHtmlPage(
    stamped: Stamped[Seq[OrderOverview]],
    pageUri: Uri)
    (implicit webServiceContext: MasterWebServiceContext, ec: ExecutionContext): Future[OrdersHtmlPage]
  =
      Future.successful(new OrdersHtmlPage(stamped, pageUri, webServiceContext))

  private def orderToTrClass(order: OrderOverview): Option[String] =
    order.state match {
      case Order.Waiting ⇒ Some("Order-Waiting")
      case _: Order.Started ⇒ Some("Order-Started")
      case Order.Finished ⇒ Some(s"Order-Finished")
      case _ ⇒ None
    }
}
