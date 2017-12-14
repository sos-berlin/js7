package com.sos.jobscheduler.master.gui.components.state

/**
  * @author Joacim Zschimmer
  */
final case class GuiState(
  //overview: Option[Either[HttpClientException.HttpFailure, MasterOverview]],
  ordersState: OrdersState,
  isFreezed: Boolean,
  isConnected: Boolean)

object GuiState {
  val Initial = GuiState(OrdersState.Empty, isFreezed = false, isConnected = false)
}
