package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderBroken, OrderDetachable, OrderForked, OrderJoined, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Fork

/**
  * @author Joacim Zschimmer
  */
object ForkExecutor extends EventInstructionExecutor
{
  type Instr = Fork

  private val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], fork: Fork) =
    Checked(
      order.ifState[Order.Fresh].map(order =>
        order.id <-: OrderStarted)
      .orElse(
        order.ifState[Order.Ready].map(order =>
          checkOrderForked(context,
            order.id <-: OrderForked(
              for (branch <- fork.branches) yield
                OrderForked.Child(branch.id, order.id / branch.id.string)))))
      .orElse(toJoined(context, order))
      .orElse(
        ifProcessedThenOrderMoved(order, context)))

  private def checkOrderForked(context: OrderContext, orderForked: KeyedEvent[OrderForked]): KeyedEvent[OrderActorEvent] = {
    val duplicates = orderForked.event.children map (_.orderId) flatMap (o => context.idToOrder.lift(o))
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost event OrderDetached
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      logger.error(problem.toString)
      orderForked.key <-: OrderBroken(problem)  // TODO Invalidate whole toEvent with order.key <-: OrderBroken
    } else
      orderForked
  }

  private def toJoined(context: OrderContext, order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Forked].flatMap(order =>
      //orderEntry.fork match {
      //  case fork: Instruction.Fork if fork isJoinableOnAgent ourAgentRefPath =>
      if (order.isAttached)
        Some(order.id <-: OrderDetachable)
      else if (order.state.childOrderIds map context.idToOrder forall context.childOrderEnded)
        Some(order.id <-: OrderJoined(Outcome.succeeded))
      else
        None)

  private[instructions] def tryJoinChildOrder(context: OrderContext, childOrder: Order[Order.State], fork: Fork)
  : Checked[Option[KeyedEvent[OrderActorEvent]]] =
    //if (childOrder.attached forall fork.isJoinableOnAgent)
    Valid(
      if (childOrder.isAttached)
        Some(childOrder.id <-: OrderDetachable)
      else
        childOrder.parent
          .flatMap(context.idToOrder.lift)
          .flatMap(parentOrder => toJoined(context, parentOrder)))
}
