package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.master.gui.data.MasterOverview
import com.sos.jobscheduler.master.gui.services.MasterApi

/**
  * @author Joacim Zschimmer
  */
final case class GuiState(
  overview: Option[Either[MasterApi.Error, MasterOverview]],
  ordersState: OrdersState,
  isConnected: Boolean)

object GuiState {
  val Initial = GuiState(None, OrdersState.Empty, isConnected = false)
}
