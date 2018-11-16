package com.sos.jobscheduler.master.gui.browser

import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.master.gui.browser.GuiRenderer._
import com.sos.jobscheduler.master.gui.browser.components.state.{AppState, GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.browser.router.Router
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.guiConfig
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

/**
  * @author Joacim Zschimmer
  */
final class GuiRenderer(
  props: GuiComponent.Props,
  stateSnapshot: StateSnapshot[GuiState],
  toggleFreezed: Callback)
{
  private def state = stateSnapshot.value

  def render =
    <.div(^.cls := "container.fluid",
      //<.header(
      //  //menuClickHtml,
      //  SideBarComponent()),
      //HtmlTag("main"),
      //<.div(^.cls := "section", ^.id :="index-banner"),
      centerVdom)

  private def centerVdom =
    <.div(^.cls := "row")(
      <.div(^.cls := "col")(
        <.div(^.cls := "page-center")(
          topLineVdom,
          Router.route(stateSnapshot))))

  private def topLineVdom = {
    val (events, clock) = eventsAndClock
    <.table(^.cls := "page-top")(
      <.tbody(
        <.tr(
          topLineleft,
          <.td(^.cls := "page-top-right")
            (events, appState, clock))))
  }

  private def eventsAndClock: (TagMod, TagMod) =
    state.ordersState.content match {
      case OrdersState.Starting ⇒
        (TagMod.empty, TagMod.empty)
      case OrdersState.FetchingContent ⇒
        (TagMod.empty,
          <.span(^.cls := "page-top-right-segment")(
            "..."))
      case OrdersState.FetchedContent(_, _, eventId, eventCount) ⇒
        val dateTime = EventId.toTimestamp(eventId).toLocaleIsoStringWithoutOffset take 19
        val events = <.span(^.cls := "page-top-right-segment unimportant")(s"$eventCount events")
        val dt =
          <.span(^.cls := "page-top-right-segment")(
            <.span(^.cls := "nowrap")(dateTime.replace("T", " ")))
            //<.span(^.cls := "hide-on-mobile unimportant")(s"$millis"))))
        (events, dt)
    }

  private def appState: TagMod =
    <.span(^.cls := "page-top-right-segment hidden-link", ^.onClick --> toggleFreezed,
                  ^.title := (if (state.appState == AppState.Freezed) "Click to keep up to date" else "Click to freeze"),
      state.appState match {
        case AppState.Freezed ⇒ <.span(^.cls := "freezed")("❄ freezed")
        case AppState.Standby ⇒ <.span(^.cls := "standby")(s"$Moon standby")
        case AppState.RequestingEvents ⇒
          state.ordersState.content match {
            case OrdersState.Starting ⇒
              "starting..."
            case OrdersState.FetchingContent ⇒
              "fetching..."
            case _ if state.isConnected ⇒
              Connected
            case _ ⇒
              state.ordersState.error match {
                case Some(error) ⇒ <.span(^.cls := "disconnected", ^.title := error)("☠️ ERROR")
                case None ⇒ <.span(^.cls := "disconnected")("⚠️ disconnected")
              }
          }
      })
}

object GuiRenderer
{
  private val Connected = VdomArray(
    <.i(^.cls := "material-icons rotate-slowly top-line")("autorenew"),
    " connected")
  val Moon = "\uD83C\uDF19"

  private lazy val topLineleft = {
    val Array(version, versionExt) = (guiConfig.buildVersion + " ").split(" ", 2)
    <.td(^.cls := "page-top-left")(
      <.span(^.cls := "page-top-left-segment")(
        <.a(^.cls := "hidden-link", ^.href := "#")(
          "JobScheduler Master ",
          <.span(^.cls := "hide-on-phone unimportant")(
            "· ",
            version,
            <.span(^.cls := "hide-on-mobile")(s" $versionExt") when versionExt.nonEmpty))),
      <.span(^.cls := "hide-on-phone page-top-left-segment experimental-monitor  unimportant")(
        "EXPERIMENTAL MONITOR"))
  }

  private def menuClickVdom =
    <.a(^.href := "#", VdomAttr("data-activates") := "nav-mobile", ^.cls := "button-collapse top-nav waves-effect waves-light circle hide-on-large-only")(
      <.i(^.cls := "material-icons")("menu"))
}
