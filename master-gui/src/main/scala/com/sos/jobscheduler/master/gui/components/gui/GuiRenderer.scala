package com.sos.jobscheduler.master.gui.components.gui

import com.sos.jobscheduler.master.gui.components.SideBarComponent
import com.sos.jobscheduler.master.gui.components.gui.GuiRenderer._
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListComponent
import com.sos.jobscheduler.master.gui.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.data.MasterOverview
import com.sos.jobscheduler.master.gui.data.event.EventId
import japgolly.scalajs.react.vdom.html_<^._

/**
  * @author Joacim Zschimmer
  */
final class GuiRenderer(state: GuiState) {

  def render = {
    <.div(
      <.header(
        menuClickHtml,
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

  private def topLineHtml =
    <.table(^.cls := "page-top-line")(
      <.tbody(
        <.tr(topLineLeftHtml, topLineRightHtml)))

  private val topLineLeftHtml = {
    val Array(version, versionExt) = (ofOverview(_.version, "…") + " ").split(" ", 2)
    <.td(^.cls := "no-padding")(
      <.span(^.cls := "nowrap")("JobScheduler Master "),
      <.span(^.cls := "nowrap")(
        version,
        <.span(^.cls := "unimportant")(s" $versionExt") when versionExt.nonEmpty),
      " ",
      <.span(^.cls := "nowrap", ^.letterSpacing := 1.px)(
        "EXPERIMENTAL VIEWER")
    )
  }

  private val topLineRightHtml =
    <.td(^.cls := "no-padding nowrap", ^.textAlign.right)(
      if (state.isConnected)
        "connected"
      else
        <.span(^.cls := "disconnected")("disconnected"),
      " ∙ ",
      state.ordersState.content match {
        case OrdersState.FetchedContent(_, _, eventId, eventCount) ⇒
          val (dt, millis) = EventId.toTimestamp(eventId).toLocaleIsoStringWithoutOffset splitAt 19
          VdomArray(
            <.span(^.cls := "nowrap unimportant")(s"$eventCount events ∙ "),
            <.span(^.cls := "nowrap")(dt.replace("T", " ")),
            <.span(^.cls := "nowrap unimportant")(s"$millis"))
        case OrdersState.StillFetchingContent ⇒
          "fetching..."
      })

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
