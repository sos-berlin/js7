package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
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

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Fork): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Fresh].map(order ⇒
      order.id <-: OrderStarted)
    .orElse(
      order.ifState[Order.Ready].map(order ⇒
        checkOrderForked(context,
          order.id <-: OrderForked(
            for (branch ← instruction.branches) yield
              OrderForked.Child(branch.id, order.id / branch.id.string, MapDiff.empty)))))
    .orElse(
      order.ifState[Order.Forked].flatMap(order ⇒
        //orderEntry.instruction match {
        //  case fork: Instruction.Fork if fork isJoinableOnAgent ourAgentPath ⇒
        if (order.isAttached)
          Some(order.id <-: OrderDetachable)  //
        else if (order.state.childOrderIds map context.idToOrder forall context.childOrderEnded)
          Some(order.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded))
        else
          None))
    .orElse(
      ifProcessedThenOrderMoved(order, context))

  private def checkOrderForked(context: OrderContext, orderForked: KeyedEvent[OrderForked]): KeyedEvent[OrderActorEvent] = {
    val duplicates = orderForked.event.children map (_.orderId) flatMap (o ⇒ context.idToOrder.lift(o))
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost event OrderDetached
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      logger.error(problem.toString)
      orderForked.key <-: OrderBroken(problem)
    } else
      orderForked
  }
}
