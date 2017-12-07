package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderForked, OrderTransitionedEvent}
import com.sos.jobscheduler.data.order.{Order, OrderEvent}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transition.Transition.NodeToTransitionableOrder
import com.sos.jobscheduler.data.workflow.transition.TransitionType.{Fork, Join, Move}
import org.scalactic.Requirements._

/**
  * @author Joacim Zschimmer
  */
object Transitions {

  def switch(order: Order[Order.Processed.type], workflow: Workflow): Option[KeyedEvent[OrderTransitionedEvent]] = {
    require(order.workflowPath == workflow.path)
    workflow.nodeToTransition.get(order.nodeId) flatMap { transition ⇒
      transition.switch(Map(order.nodeId → order))
    }
  }

  implicit class ExecutableTransition(val underlying: Transition) extends AnyVal {
    import underlying.{fromForkedNodeId, fromProcessedNodeIds, outlets, toNodeIds, transitionType}

    def switch(nodeToOrder: NodeToTransitionableOrder): Option[KeyedEvent[OrderTransitionedEvent]] =
      canSwitch(nodeToOrder) option {
        val inputOrders = (fromForkedNodeId ++: fromProcessedNodeIds) map nodeToOrder
        transitionType.result(inputOrders, outlets) match {
          case Move(outlet) ⇒
            require(inputOrders.size == 1)
            val orderId = inputOrders.head.id
            require(outlets contains outlet)
            KeyedEvent(OrderEvent.OrderMoved(outlet.nodeId))(orderId)

          case Fork(children) ⇒
            require(inputOrders.size == 1)
            require(children.nonEmpty)
            val orderId = inputOrders.head.id
            KeyedEvent(OrderEvent.OrderForked(
              children.map { case Fork.Child(outlet, childOrderId, payload) ⇒
                require(outlets contains outlet)
                OrderForked.Child(childOrderId, outlet.nodeId, payload)
              })
            )(orderId)

          case Join(payload) ⇒
            require(toNodeIds.size == 1)
            val forkedOrder = inputOrders.head
            KeyedEvent(OrderEvent.OrderJoined(
              toNodeId = toNodeIds.head,
              MapDiff.diff(forkedOrder.variables, payload.variables),
              forkedOrder.outcome)
            )(forkedOrder.id)
        }
      }

    def canSwitch(nodeToFragment: NodeToTransitionableOrder): Boolean =
      fromProcessedNodeIds forall nodeToFragment.isDefinedAt
  }
}
