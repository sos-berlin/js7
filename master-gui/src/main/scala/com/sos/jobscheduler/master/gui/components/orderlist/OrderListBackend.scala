package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.WorkflowScript.FlatStatement
import com.sos.jobscheduler.data.workflow.{NodeKey, WorkflowScript}
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.common.Renderers.forTable.orderStateToVdom
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListBackend._
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListComponent.Props
import com.sos.jobscheduler.master.gui.components.state.OrdersState
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import com.sos.jobscheduler.master.gui.router.Router
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidUpdate
import japgolly.scalajs.react.extra.{OnUnmount, Reusability}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import org.scalajs.dom.{MouseEvent, window}
import scala.math.min

/**
  * @author Joacim Zschimmer
  */
private[orderlist] final class OrderListBackend(scope: BackendScope[Props, Unit])
extends OnUnmount {

  private val highlighter = new Highlighter
  private val orderSelector = new OrderSelector(scope, highlighter)

  def componentDidUpdate: (ComponentDidUpdate[Props, Unit, OrderListBackend]) ⇒ Callback =
    _ ⇒ Callback {
      highlighter.componentDidUpdate()
    }

  def onMouseMoved(event: MouseEvent): Callback =
    orderSelector.onMouseMoved(event)

  def render(props: Props): VdomElement = {
    val ordersState = props.ordersStateSnapshot.value
    ordersState.content match {
      case Initial ⇒
        <.div()

      case FetchingContent ⇒
        <.div(<.i("Fetching orders..."))

      case content: FetchedContent ⇒
        val sequence = content.workflowToOrderSeq.getOrElse(props.workflow.path, Array.empty)
        <.div(
          <.div(^.float := "left", ^.cls := "sheet orders-sheet")(
            <.div(^.cls := "sheet-headline")(
              props.workflow.path),
            <.div(sequence.length.toString, " orders"),
            ordersState.error.whenDefined(error ⇒ <.span(^.cls := "error")(error)),
            renderWorkflowContent(props.workflow, content))/*,
            <.div(^.cls := "orders-preview")(
            ordersState.selectedOrder.whenDefined(orderId ⇒
              idToOrder.get(orderId).whenDefined(entry ⇒
                OrderComponent(entry, idToOrder(_).order, props.workflow.script, isOwnPage = false))))*/)
    }
  }

  private def renderWorkflowContent(workflow: WorkflowScript.Named, content: FetchedContent) = {
    val statementWithPosition = workflow.script.flatten.collect { case o: FlatStatement.Node ⇒ o } .zipWithIndex.map { case (o, i) ⇒ o → nodeTop(i) }
    <.div(^.cls := "Workflow-content")(
      statementWithPosition.toVdomArray { case (flat, position) ⇒
        <.div(^.key := flat.statement.node.id.string, ^.position := "absolute", ^.top := position)(
          <.div(^.cls := "orders-Node",
            NodeHeadComponent(flat.statement)))
      }, {
        val orderWithPosition = for {
          (flat, nodePosition) ← statementWithPosition
          (orderEntry, i) ← {
            val orderIds = content.nodeKeyToOrderIdSeq(NodeKey(workflow.path, flat.statement.node.id))
            val n = min(orderIds.length, OrderPerNodeLimit)
            Array.tabulate[OrderEntry](n)(i ⇒ content.idToEntry(orderIds(i))).zipWithIndex
          }
        } yield (orderEntry, i, nodePosition)
        orderWithPosition.sortBy(_._1.id)  // Sort to allow React to identify the previous orders
          .toVdomArray { case (orderEntry, orderIndex, nodePosition) ⇒
            <.div(^.key := orderEntry.id.string, ^.cls := "orders-Order-moving", ^.top := nodePosition, ^.left := orderLeft(orderIndex),
              LittleOrderComponent(orderEntry))
          }
      })
  }

  private val LittleOrderComponent = ScalaComponent.builder[OrdersState.OrderEntry]("Little-Order")
    .render_P {
      case OrdersState.OrderEntry(order, _, lastOutput, updatedAt) ⇒
        orderSelector.updateOrder(order)
        val selectedClass = orderSelector.cssClass(order.id)
        val highlighted = updatedAt > highlighter.lastHighlightedAt && selectedClass.isEmpty
        <.div(^.id := OrderSelector.elementId(order.id),
              ^.cls := "sheet z-depth-2 orders-Order " + orderToClass(order) + selectedClass + (if (highlighted) "orders-Order-changed" else ""),
          <.div(^.cls := "orders-Order-OrderId", order.id.string),
            order.state match {
              case _: Order.Scheduled | Order.StartNow ⇒
                <.div(^.cls := "orders-Order-compact",
                  order.state)

              case Order.InProcess ⇒
                <.div(^.cls := "orders-Order-compact",
                  orderStateToSymbol(order.state), " ",
                  lastOutput getOrElse order.state.toString: String)

              case _ ⇒
                <.div(
                  <.span(^.cls := "orders-Order-Outcome", order.outcome),
                  <.span(^.cls := "orders-Order-State", order.state))
            })
        .ref {
          case null ⇒
          case elem ⇒
            elem.onmouseover = _ ⇒ orderSelector.onMouseOverRow(order)
            elem.onmouseout = _ ⇒ orderSelector.onMouseOutRow(order.id)
            elem.onclick = _ ⇒ window.document.location.assign(Router.hash(order.id))
            if (highlighted) highlighter.onChanged(order.id, elem)
        }
    }
    .configure {
      implicit val orderEntryReuse = Reusability.byRef[OrdersState.OrderEntry]
      Reusability.shouldComponentUpdate
    }
    .build

  private def orderToClass(order: Order[Order.State]): String =
    order.state match {
      case _: Order.NotStarted ⇒ "Order-NotStarted "
      case Order.InProcess     ⇒ "Order-InProcess "
      case Order.Finished      ⇒ "Order-Finished "
      case _                   ⇒ ""
    }

  private val NodeHeadComponent = ScalaComponent.builder[WorkflowScript.NodeStatement]("WorkflowScript.NodeStatement")
    .render_P { stmt ⇒
      <.div(^.cls := "orders-Node-head sheet z-depth-1",
        <.div(^.cls := "orders-Statement",
          stmt match {
            case stmt: WorkflowScript.Job ⇒
              VdomArray(
                <.div(stmt.nodeId.string, ": "),
                <.div("job ", <.span(^.cls := "orders-Node-Job", stmt.job.jobPath.string)),
                <.div("at ", <.span(^.cls := "orders-Node-Agent", stmt.job.agentPath.string), ";"))

            case WorkflowScript.End(_) ⇒
              s"$stmt;"
          }))
    }
    .configure {
      implicit val orderEntryReuse = Reusability.byRef[WorkflowScript.NodeStatement]
      Reusability.shouldComponentUpdate
    }
    .build

  private val NodeComponent = ScalaComponent.builder[NodeProps]("Workflow.Node")
    .render_P { props ⇒
      val stmt = props.flat.statement
      <.div(^.cls := "orders-Node")(
        NodeHeadComponent.withKey(stmt.node.id.string)(stmt),
        TagMod(
          props.orderEntries.toVdomArray(entry ⇒
            LittleOrderComponent(entry)),
          <.div(^.cls := "orders-Order-more",
            <.i(s"(${props.notShown} more orders not shown)")
          ) when props.notShown > 0))
    }
    .configure {
      implicit val orderEntryReuse = Reusability.by_==[NodeProps]
      Reusability.shouldComponentUpdate
    }
    .build

  private case class NodeProps(flat: FlatStatement.Node, orderEntries: Array[OrderEntry], notShown: Int) {
    override def equals(o: Any) = o match {
      case o: NodeProps ⇒ (flat eq o.flat) &&
        notShown != o.notShown &&
        orderEntries.length == o.orderEntries.length && {
          var i = 0
          while (i < orderEntries.length && (orderEntries(i) eq o.orderEntries(i))) {
            i += 1
          }
          i == orderEntries.length
        }
      case _ ⇒ false
    }
  }
}

object OrderListBackend {
  private val OrderPerNodeLimit = 20
  private def nodeTop(i: Int) = (120 + 80*i).px
  private def orderLeft(i: Int) = (60 + 38*i).ex
}
