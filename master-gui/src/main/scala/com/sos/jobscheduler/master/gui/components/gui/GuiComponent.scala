package com.sos.jobscheduler.master.gui.components.gui

import com.sos.jobscheduler.master.gui.components.state.GuiState
import japgolly.scalajs.react.ScalaComponent

/**
  * @author Joacim Zschimmer
  */
object GuiComponent {

  def apply() = reactComponent()

  private val reactComponent = ScalaComponent.builder[Unit]("GUI")
    .initialState[GuiState](GuiState.Initial)
    .renderBackend[GuiBackend]
    .componentDidMount(_.backend.mount())
    .componentWillUnmount(_.backend.unmount())
    .build
}
