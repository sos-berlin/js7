package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderTransitionedEvent}
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentRestarted
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
final class WorkflowProcess(workflowScript: Workflow, idToOrder: PartialFunction[OrderId, Order[Order.State]]) {

  def tryExecuteInstruction(order: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] =
    order.outcome match {
      case Outcome.Bad(AgentRestarted) ⇒
        Some(order.id <-: OrderMoved(order.position.nr))  // Repeat
      case _ ⇒
        tryExecuteInstruction2(order)
    }

  private def tryExecuteInstruction2(order: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] =
    (workflowScript.instruction(order.position), order.state) match {
      case (_: Instruction.Job, Order.Processed) ⇒
        Some(order.id <-: OrderMoved(workflowScript.nextInstructionNr(order.position)))

      case (Instruction.Gap, Order.Ready) ⇒  // Replacement for a Job running on a different Agent
        Some(order.id <-: OrderDetachable)

      case (_: Instruction.End, Order.Ready) ⇒
        order.position.dropChild match {
          case None ⇒
            if (order.isAttachedToAgent)
              Some(order.id <-: OrderDetachable)
            else
              Some(order.id <-: OrderFinished)

          case Some(returnPosition) ⇒
            workflowScript.instruction(returnPosition) match {
              case _: Instruction.ForkJoin ⇒
                //if (order.attachedToAgent forall forkjoin.isJoinableOnAgent)
                if (order.isAttachedToAgent)
                  Some(order.id <-: OrderDetachable)
                else
                  order.parent map idToOrder flatMap tryJoinParent
              case _ ⇒ None
            }
        }

      case (forkJoin: Instruction.ForkJoin, Order.Ready) ⇒
        Some(order.id <-: OrderForked(
          for (branch ← forkJoin.branches) yield
            OrderForked.Child(branch.id, order.id / branch.id, MapDiff.empty)))

      case (_: Instruction.ForkJoin, _: Order.Join) ⇒
        //orderEntry.instruction match {
        //  case forkJoin: Instruction.ForkJoin if forkJoin isJoinableOnAgent ourAgentPath ⇒
        if (order.isAttachedToAgent)
          Some(order.id <-: OrderDetachable)
        else
          order.ifState[Order.Join] flatMap tryJoinParent

      case _ ⇒
        None
    }

  private def tryJoinParent(parent: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] =
    parent.ifState[Order.Join] flatMap (parent ⇒
      parent.state.joinOrderIds map idToOrder forall childOrderEnded(parent) option
        (parent.id <-: OrderJoined(workflowScript.nextInstructionNr(parent.position), MapDiff.empty, Outcome.Good(true))))

  private def childOrderEnded(parentOrder: Order[Order.Join])(order: Order[Order.State]): Boolean =
    workflowScript.instruction(order.position).isInstanceOf[Instruction.End] &&
    order.state == Order.Ready &&
    order.position.dropChild.contains(parentOrder.position) &&
    order.attachedTo == parentOrder.attachedTo
}
