package com.sos.jobscheduler.core.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderCancelationMarked, OrderCanceled, OrderDetachable, OrderMoved}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.{End, Goto, IfNonZeroReturnCodeGoto}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{Instruction, OrderContext, Workflow, WorkflowId}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  idToWorkflow: WorkflowId ⇒ Checked[Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  private val context = new OrderContext {
    // This and idToOrder are mutable, do not use in Future !!!
    def idToOrder                                   = OrderEventSource.this.idToOrder
    def childOrderEnded(order: Order[Order.State])  = OrderEventSource.this.childOrderEnded(order)
    def instruction(position: WorkflowPosition)     = OrderEventSource.this.instruction(position)
  }

  private def childOrderEnded(order: Order[Order.State]): Boolean =
    order.parent flatMap idToOrder.lift match {
      case Some(parentOrder) ⇒
        instruction(order.workflowPosition).isInstanceOf[End] &&
          order.state == Order.Ready &&
          order.position.dropChild.contains(parentOrder.position) &&
          order.attachedState == parentOrder.attachedState
      case _ ⇒ false
    }

  def nextEvent(orderId: OrderId): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    idToOrder.checked(orderId) flatMap (order ⇒
      canceledEvent(order) match {
        case Some(event) ⇒
          Valid(Some(order.id <-: event))
        case None ⇒
          InstructionExecutor.toEvent(instruction(order.workflowPosition), order, context) match {
            case Some(oId <-: (moved: OrderMoved)) ⇒
              applyMoveInstructions(oId, moved) map Some.apply

            case o ⇒ Valid(o)
          }
      })

  /** Returns `Some(OrderDetachable | OrderCanceled)` iff order is marked as cancelable and order is in a cancelable state. */
  private def canceledEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    if (order.cancelationMarked && isOrderInCancelableState(order))
      (order.isAttached ? OrderDetachable) orElse order.isDetached ? OrderCanceled
    else
      None

  /** Returns a `Valid(Some(OrderCanceled | OrderCancelationMarked))` iff order is not already marked as cancelable. */
  def cancel(orderId: OrderId, isAgent: Boolean): Checked[Option[OrderActorEvent]] =
    idToOrder.checked(orderId) flatMap (order ⇒
      if (order.parent.isDefined)  // Should not happen when !isStarted
        Invalid(Problem.eager(s"CancelOrder(${orderId.string}): A child order cannot be canceled"))
      else
        Valid(
          !order.cancelationMarked ? (
            if (isAgent)
              if (isOrderInCancelableState(order))
                OrderDetachable
              else
                OrderCancelationMarked
            else
              if (order.isDetached && isOrderInCancelableState(order))
                OrderCanceled
              else
                OrderCancelationMarked)))

  private def isOrderInCancelableState(order: Order[Order.State]): Boolean =
    (order.isState[Order.FreshOrReady] || order.isState[Order.Stopped] || order.isState[Order.Broken]) &&
      (order.isDetached || order.isAttached) &&
      order.parent.isEmpty &&
      order.position.branchPath.isEmpty &&
      !instruction(order.workflowPosition).isInstanceOf[End]  // End reached? Then normal OrderFinished (not OrderCanceled)

  private def applyMoveInstructions(orderId: OrderId, orderMoved: OrderMoved): Checked[KeyedEvent[OrderMoved]] =
    for {
      o ← idToOrder(orderId).checkedState[Order.Processed]/*should be*/
      pos ← applyMoveInstructions(o.withPosition(orderMoved.to))
    } yield orderId <-: OrderMoved(pos)

  private[workflow] def applyMoveInstructions(order: Order[Order.Processed]): Checked[Position] =
    applyMoveInstructions(order, Nil) map {
      case Some(n) ⇒ n
      case None ⇒ order.position
    }

  @tailrec
  private def applyMoveInstructions(order: Order[Order.Processed], visited: List[Position]): Checked[Option[Position]] =
    applySingleMoveInstruction(order) match {
      case Some(position) ⇒
        if (visited contains position)
          Invalid(Problem(s"${order.id} is in a workflow loop: " +
            visited.reverse.map(pos ⇒ pos + " " +
              idToWorkflow(order.workflowId).orThrow.labeledInstruction(pos).toString.truncateWithEllipsis(50)).mkString(" --> ")))
        else
          applyMoveInstructions(order.withPosition(position), position :: visited)
      case None ⇒ Valid(Some(order.position))
    }

  private def applySingleMoveInstruction(order: Order[Order.Processed]): Option[Position] =
    idToWorkflow(order.workflowId).toOption flatMap { workflow ⇒
      workflow.instruction(order.position) match {
        case Goto(label) ⇒
          workflow.labelToPosition(order.position.branchPath, label)

        case IfNonZeroReturnCodeGoto(label) ⇒
          if (order.state.outcome.isFailed)
            workflow.labelToPosition(order.position.branchPath, label)
          else
            Some(order.position.increment)

        case instr: Instruction ⇒
          InstructionExecutor.nextPosition(context, order, instr)

        //case _: End if order.position.isNested ⇒
        //  order.position.dropChild flatMap (returnPosition ⇒
        //    workflow.instruction(returnPosition) match {
        //      case _: If ⇒
        //        nextPosition(order withPosition returnPosition)
        //      case _ ⇒
        //        None
        //    })

        case _ ⇒ None
      }
  }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow(workflowPosition.workflowId).orThrow.instruction(workflowPosition.position)
}
