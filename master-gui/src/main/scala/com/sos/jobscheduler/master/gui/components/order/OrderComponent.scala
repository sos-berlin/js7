package com.sos.jobscheduler.master.gui.components.order

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.gui.common.Renderers._
import com.sos.jobscheduler.master.gui.common.Utils.ops.maybeToTagMod
import com.sos.jobscheduler.master.gui.common.Utils.{toVdomArray, vdomNodes}
import com.sos.jobscheduler.master.gui.components.state.OrdersState.OrderEntry
import io.circe.syntax.EncoderOps
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import org.scalajs.dom

/**
  * @author Joacim Zschimmer
  */
object OrderComponent {
  def apply(orderId: OrderId, maybeOrderEntry: Option[OrderEntry]) =
    scalaComponent(Props(orderId, maybeOrderEntry))

  private val scalaComponent = ScalaComponent.builder[Props]("Order")
    .renderBackend[OrderBackend]
    .componentWillMount(o ⇒ o.backend.componentWillMount())
    .build

  private[order] final class OrderBackend(scope: BackendScope[Props, Unit]) {
    def componentWillMount() =
      Callback {
        dom.window.scroll(0, 0)
      }

    def render(props: Props): VdomElement =
      <.div(
        <.h4("Order ", <.span(^.whiteSpace := "nowrap")(props.orderId.string.toString)),
        props.maybeOrderEntry match {
          case Some(entry) ⇒ renderOrderEntry(entry)
          case None ⇒ "—"
        })

    private def renderOrderEntry(orderEntry: OrderEntry): TagMod =
      <.div(
        renderCore(orderEntry),
        renderVariables(orderEntry.order.variables),
        renderOutput(orderEntry.output))

    private def renderCore(orderEntry: OrderEntry): VdomNode =
      <.table(^.cls := "order-table")(
        <.tbody(
          for (parent ← orderEntry.order.parent) yield
            <.tr(
              <.td(^.cls := "order-td-field")("Parent"),
              <.td(^.cls := "order-td-value")(parent)),
          <.tr(
            <.td(^.cls := "order-td-field")("Node"),
            <.td(^.cls := "order-td-value")(orderEntry.order.workflowPath.string + " : " + orderEntry.order.nodeId)),
          <.tr(
            <.td(^.cls := "order-td-field")("Outcome"),
            <.td(^.cls := "order-td-value")(orderEntry.order.outcome)),
          <.tr(
            <.td(^.cls := "order-td-field")("State"),
            <.td(^.cls := "order-td-value")(orderEntry.order.state)),
          <.tr(
            <.td(^.cls := "order-td-field")("Attached to"),
            <.td(^.cls := "order-td-value")(orderEntry.order.attachedTo))))

    private def renderVariables(variables: Map[String, String]): VdomNode =
      vdomNodes(
        <.h5(^.marginTop := 50.px)(
          "Variables"),
          if (variables.isEmpty)
            <.i("— no variables —")
          else
            toVdomArray(
              for ((k, v) ← variables.toVector.sortBy(_._1)) yield
                <.div(^.cls := "order-variable")(s"$k = ${v.asJson.toString.replace(" ", "·")}")))

    private def renderOutput(lines: Vector[String]) =
      vdomNodes(
        <.h5(^.marginTop := 50.px)(
          "Output"),
        toVdomArray(for (line ← lines) yield <.div(^.cls := "log")(line)))
  }

  final case class Props(orderId: OrderId, maybeOrderEntry: Option[OrderEntry])
}

