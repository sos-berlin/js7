package com.sos.jobscheduler.core.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.core.problems.{CancelChildOrderProblem, CancelStartedOrderProblem}
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAwoke, OrderCancelationMarked, OrderCanceled, OrderCatched, OrderDetachable, OrderFailed, OrderMoved, OrderStopped}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.{End, Goto, IfNonZeroReturnCodeGoto}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  idToWorkflow: WorkflowId => Checked[Workflow],
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
      case Some(parentOrder) =>
        instruction(order.workflowPosition).isInstanceOf[End] &&
          order.state == Order.Ready &&
          order.position.dropChild.contains(parentOrder.position) &&
          order.attachedState == parentOrder.attachedState
      case _ => false
    }

  def nextEvent(orderId: OrderId): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    idToOrder.checked(orderId) flatMap (order =>
      awokeEvent(order) orElse
      canceledEvent(order) match {
        case Some(event) =>
          Valid(Some(order.id <-: event))
        case None =>
          InstructionExecutor.toEvent(instruction(order.workflowPosition), order, context) match {
            case Valid(Some(oId <-: (moved: OrderMoved))) =>
              applyMoveInstructions(oId, moved) map Some.apply

            case Valid(Some(oId <-: OrderFailed(outcome))) =>  // OrderFailed is used internally only
              assert(oId == orderId)
              catchPosition(orderId) match {
                case None => Valid(Some(oId <-: OrderStopped(outcome)))
                case Some(pos) =>
                  applyMoveInstructions(order.withPosition(pos))
                    .flatMap(pos => Valid(Some(oId <-: OrderCatched(outcome, pos))))
              }

            case o => o
          }
      })

  private def catchPosition(orderId: OrderId): Option[Position] =
    for {
      order <- idToOrder.checked(orderId).toOption
      workflow <- idToWorkflow(order.workflowId).toOption
      position <- workflow.findCatchPosition(order.position)
    } yield position

  private def awokeEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    order.ifState[Order.DelayedAfterError]
      .map(_ => OrderAwoke)  // AgentOrderKeeper has already checked time

  /** Returns `Some(OrderDetachable | OrderCanceled)` iff order is marked as cancelable and order is in a cancelable state. */
  private def canceledEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    if (isOrderCancelable(order))
      (order.isAttached ? OrderDetachable) orElse (order.isDetached ? OrderCanceled)
    else
      None

  /** Returns a `Valid(Some(OrderCanceled | OrderCancelationMarked))` iff order is not already marked as cancelable. */
  def cancel(orderId: OrderId, mode: CancelMode, isAgent: Boolean): Checked[Option[OrderActorEvent]] =
    idToOrder.checked(orderId) flatMap (order =>
      if (order.parent.isDefined)
        Invalid(CancelChildOrderProblem(orderId))
      else if (mode == CancelMode.NotStarted && order.isStarted)
        Invalid(CancelStartedOrderProblem(orderId))
      else if (order.cancel.nonEmpty)
        Valid(None)  // Already marked as being canceled
      else if (isAgent)
        if (isOrderCancelable(order, mode))
          Valid(Some(OrderDetachable))
        else
          Valid(Some(OrderCancelationMarked(mode)))
      else if (order.isDetached && isOrderCancelable(order, mode))
        Valid(Some(OrderCanceled))
      else
        Valid(Some(OrderCancelationMarked(mode))))

  def isOrderCancelable(order: Order[Order.State]): Boolean =
    order.cancel match {
      case None => false
      case Some(mode) => isOrderCancelable(order, mode)
    }

  private def isOrderCancelable(order: Order[Order.State], mode: CancelMode): Boolean =
    (order.isState[Order.Fresh] ||
      (mode == CancelMode.FreshOrStarted && (order.isState[Order.Ready] || order.isState[Order.Stopped] || order.isState[Order.Broken]))
    ) &&
      (order.isDetached || order.isAttached) &&
      order.parent.isEmpty &&
      !instruction(order.workflowPosition).isInstanceOf[End]  // End reached? Then normal OrderFinished (not OrderCanceled)

  private def applyMoveInstructions(orderId: OrderId, orderMoved: OrderMoved): Checked[KeyedEvent[OrderMoved]] =
    for (pos <- applyMoveInstructions(idToOrder(orderId).withPosition(orderMoved.to)))
      yield orderId <-: OrderMoved(pos)

  private[workflow] def applyMoveInstructions(order: Order[Order.State]): Checked[Position] =
    applyMoveInstructions(order, Nil) map {
      case Some(n) => n
      case None => order.position
    }

  @tailrec
  private def applyMoveInstructions(order: Order[Order.State], visited: List[Position]): Checked[Option[Position]] =
    applySingleMoveInstruction(order) match {
      case o @ Invalid(_) => o
      case Valid(Some(position)) =>
        if (visited contains position)
          Invalid(Problem(s"${order.id} is in a workflow loop: " +
            visited.reverse.map(pos => pos + " " +
              idToWorkflow(order.workflowId).orThrow.labeledInstruction(pos).toString.truncateWithEllipsis(50)).mkString(" --> ")))
        else
          applyMoveInstructions(order.withPosition(position), position :: visited)
      case Valid(None) => Valid(Some(order.position))
    }

  private def applySingleMoveInstruction(order: Order[Order.State]): Checked[Option[Position]] =
    idToWorkflow(order.workflowId) flatMap { workflow =>
      workflow.instruction(order.position) match {
        case Goto(label, _) =>
          workflow.labelToPosition(order.position.branchPath, label) map Some.apply

        case IfNonZeroReturnCodeGoto(label, _) =>
          if (order.lastOutcome.isFailed)
            workflow.labelToPosition(order.position.branchPath, label) map Some.apply
          else
            Valid(Some(order.position.increment))

        case instr: Instruction =>
          InstructionExecutor.nextPosition(context, order, instr)

        //case _: End if order.position.isNested =>
        //  order.position.dropChild flatMap (returnPosition =>
        //    workflow.instruction(returnPosition) match {
        //      case _: If =>
        //        nextPosition(order withPosition returnPosition)
        //      case _ =>
        //        None
        //    })

        case _ => Valid(None)
      }
  }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow(workflowPosition.workflowId).orThrow.instruction(workflowPosition.position)
}
