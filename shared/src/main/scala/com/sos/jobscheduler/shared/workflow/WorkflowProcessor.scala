package com.sos.jobscheduler.shared.workflow

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved}
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentRestarted
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Instruction.{End, ForkJoin, Gap, Goto, IfErrorGoto, IfReturnCode, Job}
import com.sos.jobscheduler.data.workflow.{Instruction, Position, Workflow, WorkflowPath, WorkflowPosition}
import com.sos.jobscheduler.shared.workflow.WorkflowProcessor._
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowProcessor(
  pathToWorkflow: PartialFunction[WorkflowPath, Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderActorEvent]] =
    nextEvent(idToOrder(orderId))

  private def nextEvent(order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] =
    (order.state, order.outcome) match {
      case (Order.Processed, Outcome.Bad(AgentRestarted)) ⇒
        Some(order.id <-: OrderMoved(order.position))  // Repeat
      case _ ⇒
        nextEvent2(order)
    }

  private def nextEvent2(order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] = {
    val workflow = pathToWorkflow(order.workflowPath)
    (order.state, workflow.instruction(order.position)) match {
      //case (_: Order.Idle/*Ready!!!*/, _: Job) ⇒
      //  Some(order.id <-: OrderProcessingStarted)  // The caller may decide to postpone the job start

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
            OrderForked.Child(branch.id, order.id / branch.id.childId, MapDiff.empty)))

      case (_: Order.Join, _: ForkJoin) ⇒
        //orderEntry.instruction match {
        //  case forkJoin: Instruction.ForkJoin if forkJoin isJoinableOnAgent ourAgentPath ⇒
        if (order.isAttachedToAgent)
          Some(order.id <-: OrderDetachable)
        else
          order.ifState[Order.Join] flatMap tryJoinParent

      case (Order.Processed, _: Job) ⇒
        for (to ← nextPosition(order)) yield
          order.id <-: OrderMoved(to)

      case (Order.InProcess, _: Job) ⇒
        None

      case (instruction, state) ⇒
        logger.trace(s"❓ $state -> $instruction")
        None
    }
  }

  private def tryJoinParent(parent: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] =
    parent.ifState[Order.Join] flatMap (parentOrder ⇒
      nextPosition(parentOrder) flatMap (next ⇒
        parentOrder.state.joinOrderIds map idToOrder forall childOrderEnded(parentOrder) option
          (parentOrder.id <-: OrderJoined(next, MapDiff.empty, Outcome.Good(true)))))

  private def childOrderEnded(parentOrder: Order[Order.Join])(order: Order[Order.State]): Boolean =
    instruction(order.workflowPosition).isInstanceOf[End] &&
      order.state == Order.Ready &&
      order.position.dropChild.contains(parentOrder.position) &&
      order.attachedTo == parentOrder.attachedTo

  private def nextPosition(order: Order[Order.State]): Option[Position] =
    applyTransitionInstructions(order withPosition order.position.increment)

  private[workflow] def applyTransitionInstructions(order: Order[Order.State]): Option[Position] =
    applyTransitionInstructions(order, Nil) match {
      case Valid(Some(n)) ⇒ Some(n)
      case Valid(None) ⇒ Some(order.position)
      case Invalid(message) ⇒
        logger.error(message) // TODO
        None
    }

  @tailrec
  private def applyTransitionInstructions(order: Order[Order.State], visited: List[Position]): Validated[String, Option[Position]] =
    applySingleTransitionInstruction(order) match {
      case Some(position) ⇒
        if (visited contains position)
          Invalid(s"${order.id} is in a workflow loop: " +
            visited.reverse.map(pos ⇒ pathToWorkflow(order.workflowPath).labeledInstruction(pos).toShortString).mkString(" -> "))
        else
          applyTransitionInstructions(order.withPosition(position), position :: visited)
      case None ⇒ Valid(Some(order.position))
    }

  private def applySingleTransitionInstruction(order: Order[Order.State]): Option[Position] = {
    val workflow = pathToWorkflow(order.workflowPath)
    workflow.instruction(order.position) match {
      case instr: IfReturnCode ⇒
        instr.nextPosition(order, idToOrder)

      case Goto(label) ⇒
        workflow.labelToPosition(order.position.parents, label)

      case IfErrorGoto(label) ⇒
        if (order.outcome.isError)
          workflow.labelToPosition(order.position.parents, label)
        else
          Some(order.position.increment)

      case _: End if order.position.isNested ⇒
        order.position.dropChild flatMap (returnPosition ⇒
          workflow.instruction(returnPosition) match {
            case _: IfReturnCode ⇒
              nextPosition(order withPosition returnPosition)
            case _ ⇒
              None
          })

      case _ ⇒ None
    }
  }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    pathToWorkflow(workflowPosition.workflowPath).instruction(workflowPosition.position)
}

object WorkflowProcessor {
  private val logger = Logger(getClass)

  sealed trait FollowUp
  object FollowUp {
    final case class Add(order: Order[Order.Ready.type]) extends FollowUp
    final case class Remove(orderId: OrderId) extends FollowUp
  }
}
