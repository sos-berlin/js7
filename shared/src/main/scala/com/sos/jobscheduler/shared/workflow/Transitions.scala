package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderForked, OrderTransitionedEvent}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transition.Transition.NodeToTransitionableOrder
import com.sos.jobscheduler.data.workflow.transition.TransitionType.{Fork, Join, Move}
import com.sos.jobscheduler.data.workflow.transitions.JoinTransition
import org.scalactic.Requirements._
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
object Transitions {

  private val logger = Logger(getClass)

  implicit class ExecutableTransition(val transition: Transition) extends AnyVal {
    import transition.{forkNodeId, fromProcessedNodeIds, outlets, toNodeIds, transitionType}

    def switch(nodeToOrder: NodeToTransitionableOrder): Option[KeyedEvent[OrderTransitionedEvent]] =
      canSwitch(nodeToOrder) option {
        val inputOrders = (forkNodeId ++: fromProcessedNodeIds) map nodeToOrder
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

    def canSwitch(nodeToOrder: NodeToTransitionableOrder): Boolean =
      (fromProcessedNodeIds ++ forkNodeId) forall nodeToOrder.isDefinedAt

    private[workflow] def joinableOrders(order: Order[Order.Transitionable], idToOrder: PartialFunction[OrderId, Order[Order.State]]): Iterable[Order[Order.Transitionable]] =
      transition.transitionType match {
        case JoinTransition ⇒
          order.state match {
            case Order.Processed if order.parent.isDefined ⇒
              forkedParentOf(order, idToOrder) match {
                case Some(forked) if forked.attachedTo == order.attachedTo ⇒
                  Vector(forked) ++ joinableChildOrders(forked, idToOrder)
                case _ ⇒
                  Nil
              }

            case _: Order.Forked ⇒
              val forked = order.castState[Order.Forked]
              Vector(order) ++ joinableChildOrders(forked, idToOrder)

            case _ ⇒
              logger.warn(s"Unexpected ${order.id} ${order.state} parent=${order.parent} in function joinableOrders", new Exception)
              Nil
          }

        case _ ⇒
          order :: Nil
      }

    private def forkedParentOf(order: Order[Order.State], idToOrder: PartialFunction[OrderId, Order[Order.State]]): Option[Order[Order.Forked]] =
      for {
        parentOrderId ← order.parent
        parent ← idToOrder.lift(parentOrderId)
        forked ← parent.ifState[Order.Forked]
      } yield forked

    private def joinableChildOrders(forked: Order[Order.Forked], idToOrder: PartialFunction[OrderId, Order[Order.State]]): Iterable[Order[Order.Transitionable]] =
      transition.transitionType match {
        case JoinTransition ⇒
          for {
            siblingOrderId ← forked.state.childOrderIds
            sibling ← idToOrder.lift(siblingOrderId)
            processed ← sibling.ifState[Order.Transitionable]
            if transition.fromNodeIds contains processed.nodeId
            if processed.attachedTo == forked.attachedTo
          } yield processed

        case _ ⇒ Nil
      }
  }
}
