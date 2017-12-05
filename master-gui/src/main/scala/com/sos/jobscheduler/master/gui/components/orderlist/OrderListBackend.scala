package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListBackend._
import com.sos.jobscheduler.master.gui.components.state.OrdersState
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidUpdate
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import org.scalajs.dom.html
import scala.collection.mutable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[orderlist] final class OrderListBackend(scope: BackendScope[OrdersState, Unit]) {

  private val highligtTrs = mutable.Buffer[html.TableRow]()

  def componentDidUpdate: (ComponentDidUpdate[OrdersState, Unit, OrderListBackend]) ⇒ Callback =
    _ ⇒ highlightChangedRows()

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
          <.table(^.cls := "bordered")(
            theadHtml,
              <.tbody(//Slow: react.CssTransitionGroup(component = "tbody", transitionName = "orderTr",
                //enterTimeout = 5000)(
                  sequence.map(idToOrder).toVdomArray(entry ⇒
                    OrderTr.withKey(entry.id.string)(entry)))))
    }

  private implicit def orderEntryReuse = OrderEntryReuse

  private val OrderTr = ScalaComponent.builder[OrdersState.Entry]("Row")
    .render_P {
      case OrdersState.Entry(order, isUpdated) ⇒
        <.tr(
          <.td(^.cls := "orderTd")(order.id),
          <.td(^.cls := "orderTd")(order.nodeKey.workflowPath),
          <.td(^.cls := "orderTd")(order.nodeKey.nodeId),
          <.td(^.cls := "orderTd")(order.outcome.toString),
          <.td(^.cls := "orderTd")(order.attachedTo),
          <.td(order.state))
        .ref { tr ⇒
          if (isUpdated) highligtTrs += tr
        }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  private def highlightChangedRows(): Callback = {
    Callback {
      val trs = highligtTrs.toVector filter (_ != null)
      highligtTrs.clear()
      for (tr ← trs) {
        tr.className = s" ${tr.className} ".replace(" orderTr-enter ", "").replace(" orderTr-enter-active ", "").trim + " orderTr-enter"
      }
      Callback {
        for (tr ← trs) {
          tr.className = s"${tr.className} orderTr-enter-active"
        }
      }.delay(100.milliseconds).runNow()
    }
  }
}

object OrderListBackend {
  private val theadHtml =
    <.thead(<.tr(
      <.th(^.width := 10.ex)("OrderId"),
      <.th(^.width := 10.ex)("Workflow"),
      <.th(^.width := 10.ex)("Node"),
      <.th(^.width := 15.ex)("Outcome"),
      <.th(^.width := 15.ex)("AttachedTo"),
      <.th("State")))

  private val OrderEntryReuse = Reusability.byRef[OrdersState.Entry]
  //private val OrderReuse = Reusability.byRef[Order[Order.State]]

  //<editor-fold defaultstate="collapsed" desc="// (No code here - does not make first rendering quicker)">
  ////OrderId = String - private implicit val OrderIdReuse = Reusability.derive[OrderId]
  ////OrderId = String - private implicit val WorkflowPathReuse = Reusability.derive[WorkflowPath]
  ////OrderId = String - private implicit val NodeIdReuse = Reusability.derive[NodeId]
  //private implicit val GoodOutcomeReuse = Reusability.derive[Outcome.Good]
  //private implicit val BadOutcomeReuse = Reusability.derive[Outcome.Bad]
  //private implicit val OutcomeReuse = Reusability.derive[Outcome]
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
  //private val WorkflowPathTd = ScalaComponent.builder[WorkflowPath]("WorkflowPath")
  //  .render_P { workflowPath ⇒
  //    <.td(^.cls := "nowrap")(workflowPath)
  //  }
  //  .build
  //
  //private val NodeIdTd = ScalaComponent.builder[NodeId]("NodeId")
  //  .render_P { nodeId ⇒
  //    <.td(^.cls := "nowrap")(nodeId)
  //  }
  //  .build
  //
  //private val OutcomeTd = ScalaComponent.builder[Outcome]("Outcome")
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
  //    WorkflowPathTd(order.nodeKey.workflowPath),
  //    NodeIdTd(order.nodeKey.nodeId),
  //    OutcomeTd(order.outcome),
  //    AgentPathTd(order.agentPath),
  //    OrderStateTd(order.state))
  //  }
  //  .configure(Reusability.shouldComponentUpdate)
  //  .build
  //</editor-fold>
}
