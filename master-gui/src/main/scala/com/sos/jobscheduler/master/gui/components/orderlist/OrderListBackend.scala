package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.master.gui.components.orderlist.OrderListBackend._
import com.sos.jobscheduler.master.gui.components.react
import com.sos.jobscheduler.master.gui.components.state.OrdersState
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import com.sos.jobscheduler.master.gui.data.Order
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, ScalaComponent}

/**
  * @author Joacim Zschimmer
  */
private[orderlist] final class OrderListBackend(scope: BackendScope[OrdersState, Unit]) {

  def render(state: OrdersState): VdomElement =
    render2(state)

  private def render2(state: OrdersState) =
    state.content match {
      case StillFetchingContent ⇒
        <.div(<.i("Fetching orders..."))

      case FetchedContent(idToOrder, sequence, _, _) ⇒
        <.div(
          <.div(^.cls := "order-count")(
            s"${idToOrder.size} orders",
            state.error map (err ⇒ VdomArray(" – ", <.span(^.cls := "error")(s"$err"))) getOrElse ""),
          <.table(^.cls := "bordered highlight")(
            theadHtml,
              react.CssTransitionGroup(component = "tbody", transitionName = "orderTr",
                transitionEnterTimeout = 5000)(
                  sequence.map(idToOrder).toVdomArray(o ⇒
                    OrderTr.withKey(o.id)(o)))))
    }
}

object OrderListBackend {
  private val theadHtml =
    <.thead(<.tr(
      <.th(^.width := 10.ex)("OrderId"),
      <.th(^.width := 10.ex)("Jobnet"),
      <.th(^.width := 10.ex)("Node"),
      <.th(^.width := 15.ex)("Outcome"),
      <.th(^.width := 15.ex)("Agent"),
      <.th("State")))

  private implicit val OrderReuse = Reusability.byRef[Order[Order.State]]

  private val OrderTr = ScalaComponent.builder[Order[Order.State]]("Row")
    .render_P { order ⇒
      <.tr(
        <.td(^.cls := "orderTd")(order.id),
        <.td(^.cls := "orderTd")(order.nodeKey.jobnetPath),
        <.td(^.cls := "orderTd")(order.nodeKey.nodeId),
        <.td(^.cls := "orderTd")(order.outcome.toString),
        <.td(^.cls := "orderTd")(order.agentPath getOrElse "–": String),
        <.td(order.state.toString))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build


  //<editor-fold defaultstate="collapsed" desc="// (No code here - does not make first rendering quicker)">
  ////OrderId = String - private implicit val OrderIdReuse = Reusability.derive[OrderId]
  ////OrderId = String - private implicit val JobnetPathReuse = Reusability.derive[JobnetPath]
  ////OrderId = String - private implicit val NodeIdReuse = Reusability.derive[NodeId]
  //private implicit val GoodOutcomeReuse = Reusability.derive[Order.Good]
  //private implicit val BadOutcomeReuse = Reusability.derive[Order.Bad]
  //private implicit val OutcomeReuse = Reusability.derive[Order.Outcome]
  //private implicit val SomeAgentPathReuse = Reusability.derive[Some[AgentPath]]
  ////private implicit val MaybeAgentPathReuse = Reusability.derive[Option[AgentPath]]
  //private implicit val orderStateReuse = Reusability.byRef[Order.State]
  //
  //private val OrderIdTd = ScalaComponent.builder[OrderId]("OrderId")
  //  .render_P { orderId ⇒
  //    <.td(^.cls := "nowrap")(orderId)
  //  }
  //  .build
  //
  //private val JobnetPathTd = ScalaComponent.builder[JobnetPath]("JobnetPath")
  //  .render_P { jobnetPath ⇒
  //    <.td(^.cls := "nowrap")(jobnetPath)
  //  }
  //  .build
  //
  //private val NodeIdTd = ScalaComponent.builder[NodeId]("NodeId")
  //  .render_P { nodeId ⇒
  //    <.td(^.cls := "nowrap")(nodeId)
  //  }
  //  .build
  //
  //private val OutcomeTd = ScalaComponent.builder[Order.Outcome]("Outcome")
  //  .render_P { outcome ⇒
  //    <.td(^.cls := "nowrap")(outcome.toString)
  //  }
  //  .build
  //
  //private val AgentPathTd = ScalaComponent.builder[Option[AgentPath]]("AgentPath")
  //  .render_P { maybeAgentPath ⇒
  //    <.td(^.cls := "nowrap")(maybeAgentPath.getOrElse("").toString)
  //  }
  //  .build
  //
  //private val OrderStateTd = ScalaComponent.builder[Order.State]("State")
  //  .render_P { state ⇒
  //    <.td(^.cls := "nowrap")(state.toString)
  //  }
  //  .build
  //
  //private val OrderTr = ScalaComponent.builder[Order[Order.State]]("Row")
  //  .render_P { order ⇒
  //    <.tr(
  //    OrderIdTd(order.id),
  //    JobnetPathTd(order.nodeKey.jobnetPath),
  //    NodeIdTd(order.nodeKey.nodeId),
  //    OutcomeTd(order.outcome),
  //    AgentPathTd(order.agentPath),
  //    OrderStateTd(order.state))
  //  }
  //  .configure(Reusability.shouldComponentUpdate)
  //  .build
  //</editor-fold>
}
