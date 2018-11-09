package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderForked, OrderJoined, OrderStopped}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.ForkJoin

/**
  * @author Joacim Zschimmer
  */
object ForkJoinExecutor extends EventInstructionExecutor
{
  type Instr = ForkJoin

  private val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: ForkJoin): Option[KeyedEvent[OrderActorEvent]] =
    order.ifState[Order.Ready].map(order ⇒
      checkOrderForked(context,
        order.id <-: OrderForked(
          for (branch ← instruction.branches) yield
            OrderForked.Child(branch.id, order.id / branch.id.string, MapDiff.empty))))
    .orElse(
      order.ifState[Order.Forked].flatMap(order ⇒
        //orderEntry.instruction match {
        //  case forkJoin: Instruction.ForkJoin if forkJoin isJoinableOnAgent ourAgentPath ⇒
        if (order.isAttachedToAgent)
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
      val problem = Problem.eager(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      logger.error(problem.toString)
      orderForked.key <-: OrderStopped(Outcome.Disrupted(problem))
    } else
      orderForked
  }
}
