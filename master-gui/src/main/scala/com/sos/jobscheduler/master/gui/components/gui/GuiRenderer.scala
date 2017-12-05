package com.sos.jobscheduler.master.gui.components.gui

import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.master.data.MasterOverview
import com.sos.jobscheduler.master.gui.components.SideBarComponent
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListComponent
import com.sos.jobscheduler.master.gui.components.state.{GuiState, OrdersState}
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^._

/**
  * @author Joacim Zschimmer
  */
final class GuiRenderer(state: GuiState, toggleFreezed: Callback) {

  def render = {
    <.div(
      <.header(
        //menuClickHtml,
        SideBarComponent()),
      //HtmlTag("main"),
      //<.div(^.cls := "section", ^.id :="index-banner"),
      centerHtml)
  }

  private def centerHtml =
    <.div(^.cls := "container")(
      <.div(^.cls := "row")(
        <.div(^.cls := "col s12")(
          topLineHtml,
          OrderListComponent(state.ordersState))))

  private def topLineHtml = {
    val longVersion = ofOverview(_.version, "…")
    val Array(version, versionExt) = (longVersion + " ").split(" ", 2)
    val left =
      <.td(^.cls := "top-left")(
        <.span(^.cls := "top-left-segment")(
          "JobScheduler Master ",
          version,
          <.span(^.cls := "unimportant")(s" $versionExt") when versionExt.nonEmpty),
        " ",
        <.span(^.cls := "top-left-segment experimental-monitor")(
          "EXPERIMENTAL MONITOR"))

    val right =
      <.td(^.cls := "top-right")(
        <.span(^.cls := "top-right-segment freeze-toggle", ^.onClick --> toggleFreezed,
          ^.title := (if (state.isFreezed) "Click to keep up to date" else "Click to freeze"),
          if (state.isFreezed)
            <.span(^.cls := "freezed")("❄ freezed")
          else if (state.isConnected)
            "connected"
          else
            <.span(^.cls := "disconnected")("⚠️ disconnected")),
        state.ordersState.content match {
          case OrdersState.FetchedContent(_, _, eventId, eventCount) ⇒
            val (dt, millis) = EventId.toTimestamp(eventId).toLocaleIsoStringWithoutOffset splitAt 19
            VdomArray(
              <.span(^.cls := "top-right-segment unimportant")(s"$eventCount events"),
              <.span(^.cls := "top-right-segment")(
                <.span(^.cls := "nowrap")(dt.replace("T", " ")),
                <.span(^.cls := "nowrap unimportant")(s"$millis")))
          case OrdersState.StillFetchingContent ⇒
            "fetching..."
        })

    <.table(^.cls := "page-top-line")(
        <.tbody(
        <.tr(left, right)))
  }

  private def ofOverview[A](f: MasterOverview ⇒ A, default: A) =
    state.overview match {
      case Some(Right(o)) ⇒ f(o)
      case _ ⇒ default
    }
}


object GuiRenderer {
  private def menuClickHtml =
    <.div(^.cls := "container")(
      <.a(^.href := "#", VdomAttr("data-activates") := "nav-mobile", ^.cls := "button-collapse top-nav waves-effect waves-light circle hide-on-large-only")(
        <.i(^.cls := "material-icons")("menu")))
}
