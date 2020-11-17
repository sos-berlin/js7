package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.OrderContext
import js7.data.order.OrderEvent.{OrderActorEvent, OrderBroken, OrderDetachable, OrderFailedCatchable, OrderForked, OrderJoined, OrderMoved, OrderStarted}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fork

/**
  * @author Joacim Zschimmer
  */
object ForkExecutor extends EventInstructionExecutor
{
  type Instr = Fork

  def toEvent(context: OrderContext, order: Order[Order.State], fork: Fork) =
    Checked(
      order.ifState[Order.Fresh].map(order =>
        order.id <-: OrderStarted)
      .orElse(
        order.ifState[Order.Ready].map(order =>
          checkOrderForked(context,
            order.id <-: OrderForked(
              for (branch <- fork.branches) yield
                OrderForked.Child(branch.id, order.id | branch.id.string)))))
      .orElse(toJoined(context, order))
      .orElse(order.ifState[Order.Processed].map(order =>
        order.id <-: (
          order.lastOutcome match {
            case _: Outcome.Succeeded =>
              OrderMoved(order.position.increment)

            case _: Outcome.NotSucceeded =>
              OrderFailedCatchable()
          }))))

  private def checkOrderForked(context: OrderContext, orderForked: KeyedEvent[OrderForked]): KeyedEvent[OrderActorEvent] = {
    val duplicates = orderForked.event.children.map(_.orderId).flatMap(o => context.idToOrder(o).toOption)
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost event OrderDetached
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      scribe.error(problem.toString)
      orderForked.key <-: OrderBroken(problem)  // TODO Invalidate whole toEvent with order.key <-: OrderBroken
    } else
      orderForked
  }

  private def toJoined(context: OrderContext, order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Forked].flatMap(order =>
      //orderEntry.fork match {
      //  case fork: Instruction.Fork if fork isJoinableOnAgent ourAgentName =>
      if (order.isAttached)
        Some(order.id <-: OrderDetachable)
      else {
        val childOrders = order.state.childOrderIds.flatMap(o => context.idToOrder(o).toOption)
        childOrders.forall(context.childOrderEnded) ? (
          order.id <-: OrderJoined(
            if (childOrders.exists(_.lastOutcome.isFailed))
              Outcome.Failed()
            else
              Outcome.succeeded))
      })

  private[workflow] def tryJoinChildOrder(context: OrderContext, childOrder: Order[Order.State], fork: Fork)
  : Option[KeyedEvent[OrderActorEvent]] =
    if (childOrder.isAttached)
      Some(childOrder.id <-: OrderDetachable)
    else
      childOrder.parent
        .flatMap(o => context.idToOrder(o).toOption)
        .flatMap(parentOrder => toJoined(context, parentOrder))
}
