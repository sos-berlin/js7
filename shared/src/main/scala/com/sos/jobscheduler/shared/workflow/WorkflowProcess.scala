package com.sos.jobscheduler.shared.workflow

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderTransitionedEvent}
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentRestarted
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Instruction.{End, ForkJoin, Gap, Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.{InstructionNr, Workflow}
import com.sos.jobscheduler.shared.workflow.WorkflowProcess._
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowProcess(workflow: Workflow, idToOrder: PartialFunction[OrderId, Order[Order.State]]) {

  def tryExecuteInstruction(order: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] =
    order.outcome match {
      case Outcome.Bad(AgentRestarted) ⇒
        Some(order.id <-: OrderMoved(order.position.nr))  // Repeat
      case _ ⇒
        tryExecuteInstruction2(order)
    }

  private def tryExecuteInstruction2(order: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] =
    (order.state, workflow.instruction(order.position)) match {
      case (Order.Ready, Gap) ⇒  // Replacement for a Job running on a different Agent
        Some(order.id <-: OrderDetachable)

      case (Order.Ready, _: End) ⇒
        order.position.dropChild match {
          case None ⇒
            if (order.isAttachedToAgent)
              Some(order.id <-: OrderDetachable)
            else
              Some(order.id <-: OrderFinished)

          case Some(returnPosition) ⇒
            workflow.instruction(returnPosition) match {
              case _: ForkJoin ⇒
                //if (order.attachedToAgent forall forkjoin.isJoinableOnAgent)
                if (order.isAttachedToAgent)
                  Some(order.id <-: OrderDetachable)
                else
                  order.parent map idToOrder flatMap tryJoinParent
              case _ ⇒ None
            }
        }

      case (Order.Ready, forkJoin: ForkJoin) ⇒
        Some(order.id <-: OrderForked(
          for (branch ← forkJoin.branches) yield
            OrderForked.Child(branch.id, order.id / branch.id, MapDiff.empty)))

      case (_: Order.Join, _: ForkJoin) ⇒
        //orderEntry.instruction match {
        //  case forkJoin: Instruction.ForkJoin if forkJoin isJoinableOnAgent ourAgentPath ⇒
        if (order.isAttachedToAgent)
          Some(order.id <-: OrderDetachable)
        else
          order.ifState[Order.Join] flatMap tryJoinParent

      case (Order.Processed, _: Job) ⇒
        for (to ← nextInstructionNr(order)) yield
          order.id <-: OrderMoved(to)

      case (instruction, state) ⇒
        logger.trace(s"❓ $state -> $instruction")
        None
    }

  private def tryJoinParent(parent: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] =
    parent.ifState[Order.Join] flatMap (parentOrder ⇒
      nextInstructionNr(parentOrder) flatMap (next ⇒
        parentOrder.state.joinOrderIds map idToOrder forall childOrderEnded(parentOrder) option
          (parentOrder.id <-: OrderJoined(next, MapDiff.empty, Outcome.Good(true)))))

  private def childOrderEnded(parentOrder: Order[Order.Join])(order: Order[Order.State]): Boolean =
    workflow.instruction(order.position).isInstanceOf[End] &&
      order.state == Order.Ready &&
      order.position.dropChild.contains(parentOrder.position) &&
      order.attachedTo == parentOrder.attachedTo

  private def nextInstructionNr(order: Order[Order.State]): Option[InstructionNr] =
    applyJumpInstructions(order.moveTo(order.position.nr.increment))

  private[workflow] def applyJumpInstructions(order: Order[Order.State]): Option[InstructionNr] =
    applyJumpInstructions(order, Nil) match {
      case Valid(Some(n)) ⇒ Some(n)
      case Valid(None) ⇒ Some(order.position.nr)
      case Invalid(message) ⇒
        logger.error(message)
        None
    }

  @tailrec
  private def applyJumpInstructions(order: Order[Order.State], visited: List[InstructionNr]): Validated[String, Option[InstructionNr]] =
      skipJumpInstruction(order) match {
        case Some(nr) ⇒
          if (visited contains nr)
            Invalid(s"${order.id} is in a Goto loop: " + visited.reverse.map(nr ⇒ workflow.labeledInstruction(nr).toShortString).mkString(" -> "))
          else
            applyJumpInstructions(order.moveTo(nr), nr :: visited)
        case None ⇒ Valid(Some(order.position.nr))
      }

  private def skipJumpInstruction(order: Order[Order.State]): Option[InstructionNr] =
    workflow.instruction(order.position) match {
      case Goto(label) ⇒
        Some(workflow.labelToNumber(order.position, label))

      case IfError(label) ⇒
        if (order.outcome.isSuccess)
          Some(order.position.nr.increment)
        else
          Some(workflow.labelToNumber(order.position, label))

      case _ ⇒ None
    }
}

object WorkflowProcess {
  private val logger = Logger(getClass)
}
