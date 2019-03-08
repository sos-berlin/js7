package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import cats.instances.option._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.instructionToExecutor
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderFinished, OrderMoved}
import com.sos.jobscheduler.data.workflow.instructions.{End, Fork}

/**
  * @author Joacim Zschimmer
  */
object EndExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: End) =
    order.position.dropChild match {
      case None =>
        Valid(
          for (order <- order.ifState[Order.Ready]) yield
            if (order.isAttached)
              order.id <-: OrderDetachable
            else
              order.id <-: OrderFinished)

      case Some(returnPosition) =>
        context.instruction(order.workflowId /: returnPosition) match {
          case _: Fork =>
            //if (order.attached forall fork.isJoinableOnAgent)
            if (order.isAttached)
              Valid(Some(order.id <-: OrderDetachable))
            else {
              val result: Option[Checked[Option[KeyedEvent[OrderActorEvent]]]] =
                for {
                  parentOrderId <- order.parent
                  parentOrder <- context.idToOrder.lift(parentOrderId)
                  fork <- Some(context.instruction(parentOrder.workflowPosition)) collect { case o: Fork => o }
                } yield ForkExecutor.toEvent(context, parentOrder, fork)
              result.sequence map (_.flatten)  // Option[Checked[Option]] => Checked[Option]
            }
          case _ =>
            Valid(Some(order.id <-: OrderMoved(returnPosition.increment)))
        }
    }

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: End) =
    Valid(
      for {
        returnPosition <- order.position.dropChild
        next <- instructionToExecutor(context.instruction(order.workflowId /: returnPosition)) match {
          case ForkExecutor => None
          case _: PositionInstructionExecutor => Some(returnPosition.increment)  // Check this first for TryInstruction !!!
          case _: EventInstructionExecutor => Some(returnPosition)
          case _ => None
        }
      } yield next)
}
