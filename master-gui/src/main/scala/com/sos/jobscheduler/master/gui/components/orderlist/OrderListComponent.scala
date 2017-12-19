package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.master.gui.components.state.OrdersState
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted

/**
  * @author Joacim Zschimmer
  */
object OrderListComponent {

  def apply(props: OrdersState): Unmounted[OrdersState, Unit, OrderListBackend] = scalaComponent(props)

  private val scalaComponent = ScalaComponent.builder[OrdersState]("OrderList")
    .renderBackend[OrderListBackend]
    .componentDidUpdate(o ⇒ o.backend.componentDidUpdate(o))
    .build
}
