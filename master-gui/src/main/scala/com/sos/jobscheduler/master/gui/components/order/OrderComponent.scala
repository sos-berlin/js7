package com.sos.jobscheduler.master.gui.components.order

import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.components.state.OrdersState.OrderEntry
import com.sos.jobscheduler.master.gui.components.state.PreparedWorkflow
import com.sos.jobscheduler.master.gui.components.workflow.WorkflowComponent
import io.circe.syntax.EncoderOps
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, ScalaComponent}
import org.scalajs.dom.window
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object OrderComponent {
  def apply(orderEntry: OrderEntry, idToOrder: OrderId ⇒ Order[Order.State], workflow: PreparedWorkflow, isOwnPage: Boolean) =
    scalaComponent(Props(orderEntry, joinOrders(orderEntry.order, idToOrder), workflow, isOwnPage))

  private val scalaComponent = ScalaComponent.builder[Props]("Order")
    .renderBackend[OrderBackend]
    .componentWillMount(o ⇒ o.backend.componentWillMount())
    .build

  private[order] final class OrderBackend(scope: BackendScope[Props, Unit]) {
    def componentWillMount() =
      scope.props map { props ⇒
        if (props.isOwnPage) {
          window.scroll(0, 0)
        }
      }

    def render(props: Props): VdomElement =
      <.div(
        <.div(^.cls := "sheet-headline")(
          "Order ", <.span(^.whiteSpace := "nowrap")(props.orderEntry.order.id.string)),
        <.div(^.float := "left")(
          renderCore(props.orderEntry)),
        <.div(^.float := "right")(
          WorkflowComponent(props.orderEntry.order.workflowPath, props.workflow, props.orderEntry.order +: props.joinOrders)),
        <.div(^.clear := "both")(
          renderVariables(props.orderEntry.order.variables)),
          renderOutput(props.orderEntry.output))

    private def renderCore(orderEntry: OrderEntry): VdomNode =
      <.table(^.cls := "order-table")(
        <.tbody(
          orderEntry.order.parent.whenDefined(showField("Parent" , _)),
          showField("State"  , orderEntry.order.state match {
            case Order.Join(children) ⇒ VdomArray(<.div("Join"), children.toVdomArray(orderId ⇒ <.div(orderId)))
            case o ⇒ o
          }),
          showField("Position"   , orderEntry.order.workflowPosition.toString),
          showField("Attached to", orderEntry.order.attachedTo.orMissing)))

    private def showField(key: String, value: VdomNode): VdomNode =
      <.tr(
        <.td(^.cls := "order-td-field")(key),
        <.td(^.cls := "order-td-value")(value))

    private def renderVariables(variables: Map[String, String]): VdomNode =
      VdomArray(
        <.h5(^.marginTop := "50px")(
          "Variables"),
          if (variables.nonEmpty)
            variables.toVector.sortBy(_._1).toVdomArray {
              case (k, v) ⇒ <.div(^.cls := "order-variable")(s"$k = ${v.asJson.toString.replace(" ", "·")}")
            }
          else
            <.i("(no variables)"))

    private def renderOutput(lines: Vector[String]) =
      VdomArray(
        <.h5(^.marginTop := "50px")(
          "Output"),
        lines.toVdomArray(line ⇒ <.div(^.cls := "log")(line)))
  }

  private def joinOrders(order: Order[Order.State], idToOrder: OrderId ⇒ Order[Order.State]): Seq[Order[Order.State]] =
    order.ifState[Order.Join] match {
      case Some(o) ⇒ o.state.joinOrderIds.map(idToOrder).flatMap(o ⇒ o +: joinOrders(o, idToOrder))
      case None ⇒ Nil
    }

  final case class Props(
    orderEntry: OrderEntry,
    joinOrders: Seq[Order[Order.State]],
    workflow: PreparedWorkflow,
    isOwnPage: Boolean)
}

