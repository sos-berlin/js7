package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.order.OrderEvent.{OrderActorEvent, OrderBroken, OrderDetachable, OrderFailedIntermediate_, OrderForked, OrderJoined, OrderMoved, OrderStarted}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fork

private[instructions] final class ForkExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Fork

  def toEvents(fork: Fork, order: Order[Order.State], state: StateView) = {
    start(order)
      .getOrElse(Checked(order
        .ifState[Order.Fresh].map(_.id <-: OrderStarted)
        .orElse(
          order.ifState[Order.Ready].map(order =>
            checkOrderForked(state,
              order.id <-: OrderForked(
                for (branch <- fork.branches) yield
                  OrderForked.Child(branch.id, order.id | branch.id.string)))))
        .orElse(toJoined(state, order))
        .orElse(order.ifState[Order.Processed].map(order =>
          order.id <-: (
            order.lastOutcome match {
              case _: Outcome.Succeeded =>
                OrderMoved(order.position.increment)

              case _ =>
                OrderFailedIntermediate_()
            })))
        .toList))
  }

  private def checkOrderForked(state: StateView, orderForked: KeyedEvent[OrderForked]): KeyedEvent[OrderActorEvent] = {
    val duplicates = orderForked.event.children.map(_.orderId).flatMap(state.idToOrder.get)
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost event OrderDetached
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      scribe.error(problem.toString)
      orderForked.key <-: OrderBroken(problem)  // TODO Invalidate whole toEvents with order.key <-: OrderBroken
    } else
      orderForked
  }

  private def toJoined(state: StateView, order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Forked].flatMap(order =>
      //orderEntry.fork match {
      //  case fork: Instruction.Fork if fork isJoinableOnAgent ourAgentPath =>
      if (order.isAttached)
        Some(order.id <-: OrderDetachable)
      else {
        val childOrders = order.state.childOrderIds.flatMap(state.idToOrder.get)
        childOrders.forall(state.childOrderEnded) ? (
          order.id <-: OrderJoined(
            if (childOrders.exists(_.lastOutcome.isFailed))
              Outcome.failed
            else
              Outcome.succeeded))
      })

  private[workflow] def tryJoinChildOrder(state: StateView, childOrder: Order[Order.State], fork: Fork)
  : Option[KeyedEvent[OrderActorEvent]] =
    if (childOrder.isAttached)
      Some(childOrder.id <-: OrderDetachable)
    else
      childOrder.parent
        .flatMap(state.idToOrder.get)
        .flatMap(parentOrder => toJoined(state, parentOrder))
}
