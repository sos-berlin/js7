package com.sos.jobscheduler.master.gui.components.orderlist

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.Instruction.{@:, ImplicitEnd}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, Position, WorkflowPath}
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.common.Renderers.forTable.orderStateToVdom
import com.sos.jobscheduler.master.gui.common.Utils.memoize
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListBackend._
import com.sos.jobscheduler.master.gui.components.orderlist.OrderListComponent.Props
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import com.sos.jobscheduler.master.gui.components.state.{OrdersState, PreparedWorkflow}
import com.sos.jobscheduler.master.gui.router.Router
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidUpdate
import japgolly.scalajs.react.extra.{OnUnmount, Reusability}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import org.scalajs.dom.window
import scala.collection.immutable.Seq
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

  def render(props: Props): VdomElement = {
    val ordersState = props.ordersStateSnapshot.value
    ordersState.content match {
      case Initial ⇒
        <.div

      case FetchingContent ⇒
        <.div(<.i("Fetching orders..."))

      case content: FetchedContent ⇒
        val sequence = content.workflowToOrderSeq.getOrElse(props.workflowPath, Vector.empty)
        <.div(
          <.div(^.cls := "sheet-headline")(
            props.workflowPath),
          <.div(sequence.length.toString, " orders"),
          ordersState.error.whenDefined(error ⇒ <.span(^.cls := "error")(error)),
          renderWorkflowContent(props.workflowPath, props.workflow, content))
    }
  }

  private def renderWorkflowContent(workflowPath: WorkflowPath, workflow: PreparedWorkflow, content: FetchedContent) = {
    val instructionsWithY = workflow.workflow.flatten.zipWithIndex.map { case (o, i) ⇒ o → nodeYpx(i) }
    val nodesVdom = instructionsWithY.toVdomArray { case (instr, y) ⇒
      <.div(^.cls := "orders-Instruction", moveElement(nodeXpx(instr._1.depth), y))(
        <.div(^.cls := "orders-Instruction-head",
          NodeHeadComponent(instr)))
    }
    val orderWithXY: Seq[(OrderEntry, Int, Int)] = for {
      ((position, _), y) ← instructionsWithY
      orderIds = content.workflowPositionToOrderIdSeq(workflowPath /: position)
      n = min(orderIds.length, OrderPerInstructionLimit)
      (orderEntry, i) ← Array.tabulate[OrderEntry](n)(i ⇒ content.idToEntry(orderIds(i))).zipWithIndex
    } yield (orderEntry, orderXpx(position.depth, i), y)
    val ordersVdom = orderWithXY
      .sortBy(_._1.id)  // Sort to allow React to identify known orders
      .toVdomArray { case (orderEntry, x, y) ⇒
        <.div(^.key := orderEntry.id.string, moveElement(x, y), ^.cls := "orders-Order-moving",
          LittleOrderComponent(orderEntry))
      }
    <.div(^.cls := "Workflow-content", nodesVdom, ordersVdom)
  }

  private val LittleOrderComponent = ScalaComponent.builder[OrdersState.OrderEntry]("Little-Order")
    .render_P {
      case OrdersState.OrderEntry(order, _, lastOutput, updatedAt) ⇒
        orderSelector.updateOrder(order)
        val selectedClass = orderSelector.cssClass(order.id)
        val highlighted = false //updatedAt > highlighter.lastHighlightedAt && selectedClass.isEmpty
        <.div(^.id := OrderSelector.elementId(order.id),
              ^.cls := "sheet z-depth-1 orders-Order " + orderToClass(order) + selectedClass + (if (highlighted) "orders-Order-changed" else ""),
              ^.title := "Double-click for details",
          <.div(^.cls := "orders-Order-OrderId", order.id.string),
            order.state match {
              case _: Order.NotStarted | _: Order.Join ⇒
                <.div(^.cls := "orders-Order-compact",
                  order.state)

              case Order.InProcess ⇒
                <.div(^.cls := "orders-Order-compact",
                  orderStateToSymbol(order.state), " ",
                  lastOutput getOrElse order.state.toString: String)

              case _ ⇒
                <.div(
                  <.span(^.cls := "orders-Order-State", order.state),
                  <.span(^.cls := "orders-Order-Outcome", order.outcome))
            })
        .ref {
          case null ⇒
          case elem ⇒
            elem.onmouseover = _ ⇒ orderSelector.onMouseOver(order)
            elem.onmouseout = _ ⇒ orderSelector.onMouseOut(order.id)
            elem.onclick = _ ⇒ orderSelector.onClick(order)
            elem.ondblclick = _ ⇒ window.document.location.assign(Router.hash(order.id))
            //if (highlighted) highlighter.onChanged(order.id, elem)
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
      case _: Order.Join       ⇒ "Order-Join "
      case Order.Finished      ⇒ "Order-Finished "
      case _                   ⇒ ""
    }

  private val NodeHeadComponent = ScalaComponent.builder[(Position, Instruction.Labeled)]("Instruction")
    .render_P {
      case (position, labels @: instruction) ⇒
          <.div(
            <.div(
              position.parents map (_.childId) mkString "/",
              " ",
              labels.map(_ + ": ").mkString),
            instruction match {
              case _: Instruction.ForkJoin ⇒
                "fork"

              case Instruction.Job(AgentJobPath(agentPath, jobPath)) ⇒
                VdomArray(
                  <.div("job ", <.span(^.cls := "orders-Instruction-Job", jobPath.string)),
                  <.div("at ", <.span(^.cls := "orders-Instruction-Agent", agentPath.string)))

              case ImplicitEnd ⇒
                "end"

              case stmt: Instruction ⇒
                stmt.toString
            })
    }
    .configure {
      implicit val orderEntryReuse = Reusability.byRef[(Position, Instruction.Labeled)]
      Reusability.shouldComponentUpdate
    }
    .build

  private case class NodeProps(labeledInstruction: Instruction.Labeled, orderEntries: Array[OrderEntry], notShown: Int) {
    override def equals(o: Any) = o match {
      case o: NodeProps ⇒ (labeledInstruction eq o.labeledInstruction) &&
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
  private val OrderPerInstructionLimit = 20
  private def nodeYpx(i: Int) = 50*i
  private def nodeXpx(depth: Int) = depthPx(depth)
  private def orderXpx(depth: Int, i: Int) = depthPx(depth) + 215 + 240*i

  private def depthPx(nesting: Int) = 35 * nesting

  private val moveElement = memoize[(Int, Int), TagMod] { case (x, y) ⇒
    ^.transform := s"translate(${x}px,${y}px)"
  }
}
