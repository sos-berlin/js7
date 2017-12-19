package com.sos.jobscheduler.master.gui

import com.sos.jobscheduler.master.gui.components.state.GuiState
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.router.BaseUrl

/**
  * @author Joacim Zschimmer
  */
object GuiComponent {

  def apply(baseUrl: BaseUrl) = _component(Props(baseUrl))

  private val _component = ScalaComponent.builder[Props]("GUI")
    .initialState[GuiState](GuiState.Initial)
    .renderBackend[GuiBackend]
    .componentDidMount(_.backend.componentDidMount())
    .componentWillUnmount(_.backend.componentWillUnmount())
    .build

  final case class Props(baseUrl: BaseUrl)
}
