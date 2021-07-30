package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.order.OrderEvent.{OrderActorEvent, OrderBroken, OrderDetachable, OrderForked, OrderJoined}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.ForkInstruction

private[workflow] object ForkInstructionExecutor
{
  private[instructions] def checkOrderIdCollisions(
    state: StateView,
    orderForked: KeyedEvent[OrderForked])
  : KeyedEvent[OrderActorEvent] = {
    val duplicates = orderForked.event.children.map(_.orderId).flatMap(state.idToOrder.get)
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost event OrderDetached
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      scribe.error(problem.toString)
      orderForked.key <-: OrderBroken(problem)  // TODO Invalidate whole toEvents with order.key <-: OrderBroken
    } else
      orderForked
  }

  private[workflow] def tryJoinChildOrder(
    state: StateView,
    childOrder: Order[Order.State],
    evidence: ForkInstruction)
  : Option[KeyedEvent[OrderActorEvent]] =
    if (childOrder.isAttached)
      Some(childOrder.id <-: OrderDetachable)
    else
      childOrder.parent
        .flatMap(state.idToOrder.get)
        .flatMap(parentOrder =>
          toJoined(state, parentOrder.asInstanceOf[Order[Order.Forked]]))

  private[workflow] def toJoined(state: StateView, order: Order[Order.Forked])
  : Option[KeyedEvent[OrderActorEvent]] =
    if (order.isAttached)
      Some(order.id <-: OrderDetachable)
    else {
      // TODO SLOW: computation time for n child orders is about the square of n
      // because for each completed child order all other child orders are checked.
      // The following imperative optimization has a small effect on speed.
      var allSucceeded = true
      var allEnded = true
      val children = order.state.children.iterator
      while (allEnded && children.hasNext) {
        val child = children.next()
        val childOrder = state.idToOrder(child.orderId)
        if (state.childOrderEnded(childOrder, parent = order)) {
          allSucceeded &&= childOrder.lastOutcome.isSucceeded
        } else
          allEnded = false
      }
      allEnded ? (
        order.id <-: OrderJoined(
          if (allSucceeded)
            Outcome.succeeded
          else
            Outcome.failed))
    }
}
