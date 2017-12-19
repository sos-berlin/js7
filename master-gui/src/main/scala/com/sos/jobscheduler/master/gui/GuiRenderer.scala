package com.sos.jobscheduler.master.gui

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.gui.GuiRenderer._
import com.sos.jobscheduler.master.gui.components.SideBarComponent
import com.sos.jobscheduler.master.gui.components.order.OrderComponent
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListComponent
import com.sos.jobscheduler.master.gui.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.services.JsBridge
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.BaseUrl
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scala.scalajs.js.URIUtils.decodeURIComponent
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class GuiRenderer(props: GuiComponent.Props, state: GuiState, toggleFreezed: Callback) {

  val base = BaseUrl.fromWindowOrigin / "master"

  val render =
    <.div(
      <.header(
        //menuClickHtml,
        SideBarComponent()),
      //HtmlTag("main"),
      //<.div(^.cls := "section", ^.id :="index-banner"),
      centerVdom)

  private lazy val centerVdom =
    <.div(^.cls := "container")(
      <.div(^.cls := "row")(
        <.div(^.cls := "col s12")(
          topLineVdom,
          route)))

  private lazy val topLineVdom = {
    val left = {
      val Array(version, versionExt) = (JsBridge.jobschedulerBuildVersion + " ").split(" ", 2)
      <.td(^.cls := "top-left")(
        <.span(^.cls := "top-left-segment")(
          "JobScheduler Master ",
          TagMod(
            <.span(^.cls := "hide-on-phone")(version),
            <.span(^.cls := "hide-on-mobile unimportant")(s" $versionExt") when versionExt.nonEmpty)),
        <.span(^.cls := "hide-on-phone top-left-segment experimental-monitor")(
          "EXPERIMENTAL MONITOR"))
    }

    val right =
      <.td(^.cls := "top-right")(
        <.span(^.cls := "top-right-segment freeze-toggle", ^.onClick --> toggleFreezed,
          ^.title := (if (state.isFreezed) "Click to keep up to date" else "Click to freeze"),
          if (state.isFreezed)
            <.span(^.cls := "freezed")("❄ freezed")
          else if (state.isFetchingState)
            "fetching..."
          else if (state.isConnected)
            "connected"
          else
            <.span(^.cls := "disconnected")("⚠️ disconnected")),
        state.ordersState.content match {
          case OrdersState.Initial ⇒
            ""
          case OrdersState.FetchingContent ⇒
            <.span(^.cls := "top-right-segment")(
              "...")
          case OrdersState.FetchedContent(_, _, eventId, eventCount) ⇒
            val (dt, millis) = EventId.toTimestamp(eventId).toLocaleIsoStringWithoutOffset splitAt 19
            VdomArray(
              <.span(^.cls := "top-right-segment unimportant")(s"$eventCount events"),
              <.span(^.cls := "top-right-segment")(
                <.span(^.cls := "nowrap")(dt.replace("T", " ")),
                <.span(^.cls := "nowrap hide-on-mobile unimportant")(s"$millis")))
        })

    <.table(^.cls := "page-top-line")(
        <.tbody(
        <.tr(left, right)))
  }

  private lazy val route: TagMod =
    dom.document.location.hash match {
      case "" | "#" ⇒
        OrderListComponent(state.ordersState)

      case hash if hash.startsWith(OrderPath) ⇒
        try {
          val orderId = OrderId(decodeURIComponent(hash.stripPrefix(OrderPath)))
          OrderComponent(orderId, state.ordersState.maybeOrderEntry(orderId))
        } catch {
          case NonFatal(t) ⇒ <.div(^.cls := "error")(s"Unrecognized URI ${dom.document.location}: ${t.toStringWithCauses}")
        }

      case _ ⇒
        <.div(^.cls := "error")(s"Unrecognized URI ${dom.document.location}")
    }
}

object GuiRenderer {
  private val OrderPath = "#order/"

  private def menuClickVdom =
    <.div(^.cls := "container")(
      <.a(^.href := "#", VdomAttr("data-activates") := "nav-mobile", ^.cls := "button-collapse top-nav waves-effect waves-light circle hide-on-large-only")(
        <.i(^.cls := "material-icons")("menu")))
}
