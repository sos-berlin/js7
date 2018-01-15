package com.sos.jobscheduler.master.gui

import com.sos.jobscheduler.master.gui.components.state.{AppState, GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.services.JsBridge.jQuery

/**
  * @author Joacim Zschimmer
  */
object ScreenBackground {
  private val connected     = "app-connected"
  private val fetching      = "app-fetching"
  private val disconnected  = "app-disconnected"
  private val freezed       = "app-freezed"

  def setScreenClass(state: GuiState): Unit = {
    jQuery("html").removeClass(connected).removeClass(fetching).removeClass(disconnected).removeClass(freezed)
      .addClass(connectionClass(state))
  }

  private def connectionClass(state: GuiState) =
    state.appState match {
      case AppState.Freezed ⇒ freezed
      case _ ⇒
        if (state.ordersState.content == OrdersState.FetchingContent) fetching
        else if (state.isConnected) connected
        else disconnected
    }
}
