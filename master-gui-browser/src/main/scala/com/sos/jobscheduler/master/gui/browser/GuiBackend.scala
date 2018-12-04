package com.sos.jobscheduler.master.gui.browser

import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.master.data.MasterCommand.CancelOrder
import com.sos.jobscheduler.master.gui.browser.ScreenBackground.setScreenClass
import com.sos.jobscheduler.master.gui.browser.components.state.OrdersState.{FetchedContent, Mark}
import com.sos.jobscheduler.master.gui.browser.components.state.{AppState, GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.browser.services.MasterApi
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback}
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom.{KeyboardEvent, raw, window}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GuiBackend(scope: BackendScope[GuiComponent.Props, GuiState]) {

  private val onDocumentVisibilityChanged: raw.Event ⇒ Unit = _ ⇒ awake.runNow()

  private val onKeyDown: KeyboardEvent ⇒ Unit = { event ⇒
    window.console.log(s"onKeyDown ${event.key}")
    val callback: Callback =
      event.key match {
        case "Escape" ⇒
          scope.state.flatMap { state ⇒
            if (state.appState != AppState.Freezed)
              toggleFreezed
            else
              Callback.empty
          }

        case "Delete" ⇒
          scope.state.flatMap { state ⇒
            state.ordersState.content match {
              case content: FetchedContent ⇒
                content.markedOrders.get(Mark.Permanent) match {
                  case None ⇒ Callback.empty
                  case Some(orderId) ⇒ MasterApi.executeCommandCallback(CancelOrder(orderId, CancelMode.FreshOrStarted))
                }
              case _ ⇒ Callback.empty
            }
          }

        case _ ⇒
          Callback.empty
      }
    callback.runNow()
  }

  private val onHashChanged = { _: raw.HashChangeEvent⇒
    //dom.window.screenX
    scope.modState(state ⇒ state.updateUriHash)
      //.memoizePositionForUri(event.oldURL))
    .runNow()
  }

  private val eventHandler = new ClassicEventHandler(scope)
  private var unmounted = false

  def componentDidMount() = start()

  private def start(): Callback =
    Callback {
      window.document.addEventListener("visibilitychange", onDocumentVisibilityChanged)
      window.document.addEventListener("keydown", onKeyDown)
      window.onhashchange = onHashChanged
    } >>
      Callback.future {
        MasterApi.login(None).map { _ ⇒
          scope.modState(o ⇒ o.copy(
            ordersState = o.ordersState.copy(
              content = OrdersState.FetchingContent))) >>
          Callback.byName(eventHandler.requestStateAndEvents)
        }.runAsync
      } map (_ ⇒ ())

  def componentWillUnmount() = Callback {
    unmounted = true
    window.document.removeEventListener("visibilitychange", onDocumentVisibilityChanged, false)
    window.document.removeEventListener("keydown", onKeyDown, false)
  }

  def componentDidUpdate(): Callback =
    for (state ← scope.state) yield setScreenClass(state)

  def render(props: GuiComponent.Props, state: GuiState): VdomElement =
    new GuiRenderer(props, StateSnapshot(state).setStateVia(scope), toggleFreezed).render

  private lazy val toggleFreezed: Callback =
    for {
      state ← scope.state
      callback ←
        if (state.appState == AppState.Freezed)
          continueRequestingEvents
        else
          scope.setState(state.copy(
            appState = AppState.Freezed))
    } yield callback

  private lazy val awake: Callback =
    for {
      state ← scope.state
      callback ←
        if (state.appState == AppState.Standby)
          continueRequestingEvents
        else
          Callback.empty
    } yield callback

  private lazy val continueRequestingEvents: Callback =
    for {
      state ← scope.state
      callback ←
        state.ordersState.content match {
          case content: OrdersState.FetchedContent if !eventHandler.isRequestingEvents ⇒
            Callback.log("☀️ Continue...") >>
              scope.modState(_.copy(
                appState = AppState.RequestingEvents)
              ) >>
              eventHandler.startRequestAndHandleEvents(after = content.eventId, forStep = state.ordersState.step)
                .delay(10.milliseconds).void   // Without delay, change of appState will not have taken effect in startRequestAndHandleEvents ???
          case _ ⇒
            Callback.empty
        }
    } yield callback
}
