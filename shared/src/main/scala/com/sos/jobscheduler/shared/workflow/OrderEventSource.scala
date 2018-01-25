package com.sos.jobscheduler.shared.workflow

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.{End, Goto, IfNonZeroReturnCodeGoto}
import com.sos.jobscheduler.data.workflow.{EventInstruction, Instruction, OrderContext, Position, PositionInstruction, Workflow, WorkflowPath, WorkflowPosition}
import com.sos.jobscheduler.shared.workflow.OrderEventSource._
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  pathToWorkflow: PartialFunction[WorkflowPath, Workflow],
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
          order.attachedTo == parentOrder.attachedTo
      case _ ⇒ false
    }

  def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderActorEvent]] = {
    val order = idToOrder(orderId)
    instruction(order.workflowPosition) match {
      case instr: EventInstruction ⇒
        instr.toEvent(order, context) flatMap {
          case oId <-: (moved: OrderMoved) ⇒
            applyMoveInstructions(oId, moved)

          case o ⇒ Some(o)
        }

      case instruction ⇒
        logger.trace(s"❓ $instruction")
        None
    }
  }

  private def applyMoveInstructions(orderId: OrderId, orderMoved: OrderMoved): Option[KeyedEvent[OrderMoved]] =
    for {
      o ← idToOrder(orderId).ifState[Order.Processed]/*should be*/
      pos ← applyMoveInstructions(o.withPosition(orderMoved.to))
    } yield orderId <-: OrderMoved(pos)

  private[workflow] def applyMoveInstructions(order: Order[Order.Processed]): Option[Position] =
    applyMoveInstructions(order, Nil) match {
      case Valid(Some(n)) ⇒ Some(n)
      case Valid(None) ⇒ Some(order.position)
      case Invalid(message) ⇒
        logger.error(message) // TODO
        None
    }

  @tailrec
  private def applyMoveInstructions(order: Order[Order.Processed], visited: List[Position]): Validated[String, Option[Position]] =
    applySingleMoveInstruction(order) match {
      case Some(position) ⇒
        if (visited contains position)
          Invalid(s"${order.id} is in a workflow loop: " +
            visited.reverse.map(pos ⇒ pathToWorkflow(order.workflowPath).labeledInstruction(pos).toShortString).mkString(" -> "))
        else
          applyMoveInstructions(order.withPosition(position), position :: visited)
      case None ⇒ Valid(Some(order.position))
    }

  private def applySingleMoveInstruction(order: Order[Order.Processed]): Option[Position] = {
    val workflow = pathToWorkflow(order.workflowPath)
    workflow.instruction(order.position) match {
      case Goto(label) ⇒
        workflow.labelToPosition(order.position.parents, label)

      case IfNonZeroReturnCodeGoto(label) ⇒
        if (order.state.outcome.isFailed)
          workflow.labelToPosition(order.position.parents, label)
        else
          Some(order.position.increment)

      case instr: PositionInstruction ⇒
        instr.nextPosition(order, context)

      //case _: End if order.position.isNested ⇒
      //  order.position.dropChild flatMap (returnPosition ⇒
      //    workflow.instruction(returnPosition) match {
      //      case _: IfReturnCode ⇒
      //        nextPosition(order withPosition returnPosition)
      //      case _ ⇒
      //        None
      //    })

      case _ ⇒ None
    }
  }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    pathToWorkflow(workflowPosition.workflowPath).instruction(workflowPosition.position)
}

object OrderEventSource {
  private val logger = Logger(getClass)
}
