package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.base.utils.Strings.TruncatedString
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.common.Renderers.forTable.orderStateToTagMod
import com.sos.jobscheduler.master.gui.common.Utils.ops.maybeToTagMod
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

  private var highligtTrs = mutable.Buffer[html.TableRow]()
  private var lastHighlightedAt = Timestamp.epochMilli

  def componentDidUpdate: (ComponentDidUpdate[OrdersState, Unit, OrderListBackend]) ⇒ Callback =
    _ ⇒ highlightChangedRows()

  def render(state: OrdersState): VdomElement =
    render2(state)

  private def render2(state: OrdersState): VdomElement =
    state.content match {
      case Initial ⇒
        <.div()

      case FetchingContent ⇒
        <.div(<.i("Fetching orders..."))

      case FetchedContent(idToOrder, sequence, _, _) ⇒
        val limit = if (sequence.length <= OrderLimit + OrderLimitMargin) sequence.length else OrderLimit
        <.div(
          <.div(^.cls := "order-count")(
            s"${sequence.size} orders",
            state.error map (err ⇒ VdomArray(" – ", <.span(^.cls := "error")(s"$err"))) getOrElse ""),
          <.table(^.cls := "bordered orders-table")(
            theadVdom,
            <.tbody(
              sequence.toIterator.take(limit).map(idToOrder).toVdomArray(entry ⇒
                OrderTr.withKey(entry.id.string)(entry)))),
          sequence.length > limit option
            <.div(<.i(s"${sequence.length - limit} more orders are not shown.")))
    }

  private implicit def orderEntryReuse = OrderEntryReuse

  private val OrderTr = ScalaComponent.builder[OrdersState.OrderEntry]("Row")
    .render_P {
      case OrdersState.OrderEntry(order, output, updatedAt) ⇒
        val cls = orderToRowClass(order)
        <.tr(
          <.td(^.cls := s"td-left cls")(order.id),
          <.td(^.cls := s"orders-td hide-on-phone $cls")(order.nodeKey.workflowPath),
          <.td(^.cls := s"orders-td $cls")(order.nodeKey.nodeId),
          <.td(^.cls := s"orders-td $cls")(order.outcome),
          <.td(^.cls := s"orders-td hide-on-phone $cls")(order.attachedTo),
          <.td(^.cls := s"orders-td $cls")(order.state),
          <.td(^.cls := s"orders-td-last-output hide-on-phone $cls")(
            output.lastOption.getOrElse("").truncateWithEllipsis(50, showLength = false)))
        .ref { tr ⇒
          if (tr != null && updatedAt > lastHighlightedAt) highligtTrs += tr
        }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  private def orderToRowClass(order: Order[Order.State]): String =
    order.state match {
      case _: Order.NotStarted ⇒ "Order-NotStarted"
      case Order.InProcess ⇒ "Order-InProcess"
      case Order.Finished ⇒ "Order-Finished"
      case _ ⇒ ""
    }

  private def highlightChangedRows(): Callback = {
    Callback {
      val trs = highligtTrs
      highligtTrs = mutable.Buffer[html.TableRow]()
      for (tr ← trs) {
        tr.className = ClassRegex.replaceAllIn(tr.className, "").trim + " orders-tr-enter"
      }
      Callback {
        for (tr ← trs) {
          tr.className = s"${tr.className} orders-tr-enter-active"
        }
        lastHighlightedAt = Timestamp.epochMilli
      }.delay(5.seconds).runNow()
    }
  }
}

object OrderListBackend {
  private val OrderLimit = 1000
  private val OrderLimitMargin = 99

  private val theadVdom =
    <.thead(<.tr(
      <.th(^.width := 10.ex, ^.cls := "orders-td td-left")("OrderId"),
      <.th(^.width := 10.ex, ^.cls := "orders-td hide-on-phone")("Workflow"),
      <.th(^.width := 10.ex, ^.cls := "orders-td")("Node"),
      <.th(^.width := 15.ex, ^.cls := "orders-td")("Outcome"),
      <.th(^.width := 15.ex, ^.cls := "hide-on-phone")("AttachedTo"),
      <.th(^.width := 15.ex, ^.cls := "orders-td")("State"),
      <.th(^.width := 15.ex, ^.cls := "orders-td-right hide-on-phone")("Last output")))

  private val ClassRegex = """\b(orders-tr-enter|orders-tr-enter-active)\b|^ *| *$""".r

  private val OrderEntryReuse = Reusability.byRef[OrdersState.OrderEntry]
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
