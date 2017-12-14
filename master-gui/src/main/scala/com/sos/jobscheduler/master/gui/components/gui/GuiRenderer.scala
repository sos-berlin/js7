package com.sos.jobscheduler.master.gui.components.gui

import com.sos.jobscheduler.data.event.EventId
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
      centerVdom)
  }

  private def centerVdom =
    <.div(^.cls := "container")(
      <.div(^.cls := "row")(
        <.div(^.cls := "col s12")(
          topLineVdom,
          OrderListComponent(state.ordersState))))
          //state.overview match {
          //  case None ⇒ <.div()
          //  case Some(Left(error)) ⇒ <.p(^.fontSize := 20.px)(error.toString)
          //  case Some(Right(_)) ⇒ OrderListComponent(state.ordersState)
          //})))

  private def topLineVdom = {
    val left =
      <.td(^.cls := "top-left")(
        <.span(^.cls := "top-left-segment")(
          "JobScheduler Master "),
          //ofOverview(_.version) match {
          //  case Left(error) ⇒ error
          //  case Right(longVersion) ⇒
          //    val Array(version, versionExt) = (longVersion + " ").split(" ", 2)
          //    TagMod(
          //      <.span(^.cls := "hide-on-phone")(version),
          //      <.span(^.cls := "hide-on-mobile unimportant")(s" $versionExt") when versionExt.nonEmpty)
          //}),
        <.span(^.cls := "hide-on-phone top-left-segment experimental-monitor")(
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

  //private def ofOverview[A](f: MasterOverview ⇒ A): Either[TagMod, A] =
  //  state.overview match {
  //    case Some(Right(o)) ⇒ Right(f(o))
  //    case Some(Left(t)) ⇒ Left(<.span(^.title := t.toString)("⚠️"))
  //    case None ⇒ Left("...")
  //  }
}


object GuiRenderer {
  private def menuClickVdom =
    <.div(^.cls := "container")(
      <.a(^.href := "#", VdomAttr("data-activates") := "nav-mobile", ^.cls := "button-collapse top-nav waves-effect waves-light circle hide-on-large-only")(
        <.i(^.cls := "material-icons")("menu")))
}
