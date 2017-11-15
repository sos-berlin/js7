package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.master.gui.components.state.OrdersState
import japgolly.scalajs.react.ScalaComponent

/**
  * @author Joacim Zschimmer
  */
object OrderListComponent {

  def apply(props: OrdersState) = scalaComponent(props)

  private val scalaComponent = ScalaComponent.builder[OrdersState]("OrderList")
    .renderBackend[OrderListBackend]
    .build
}
