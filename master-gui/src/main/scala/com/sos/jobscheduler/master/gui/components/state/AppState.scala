package com.sos.jobscheduler.master.gui.components.state

/**
  * @author Joacim Zschimmer
  */
sealed trait AppState

object AppState {
  case object RequestingEvents extends AppState
  case object Freezed extends AppState
  case object Standby extends AppState
}
