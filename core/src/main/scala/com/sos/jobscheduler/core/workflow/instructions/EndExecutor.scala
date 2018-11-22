package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.core.workflow.instructions.InstructionExecutor.instructionToExecutor
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderFinished}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.{End, ForkJoin}
import com.sos.jobscheduler.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object EndExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = End

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: End): Option[KeyedEvent[OrderActorEvent]] =
    order.position.dropChild match {
      case None ⇒
        for (order ← order.ifState[Order.Ready]) yield
          if (order.isAttached)
            order.id <-: OrderDetachable
          else
            order.id <-: OrderFinished

      case Some(returnPosition) ⇒
        context.instruction(order.workflowId /: returnPosition) match {
          case _: ForkJoin ⇒
            //if (order.attached forall forkjoin.isJoinableOnAgent)
            if (order.isAttached)
              Some(order.id <-: OrderDetachable)
            else
              for {
                parentOrderId ← order.parent
                parentOrder ← context.idToOrder.lift(parentOrderId)
                forkJoin ← Some(context.instruction(parentOrder.workflowPosition)) collect { case o: ForkJoin ⇒ o }
                event ← ForkJoinExecutor.toEvent(context, parentOrder, forkJoin)
              } yield event
          case _ ⇒ None
        }
    }

  def nextPosition(context: OrderContext, order: Order[Order.Processed], instruction: End): Option[Position] =
    for {
      returnPosition ← order.position.dropChild
      next ← instructionToExecutor(context.instruction(order.workflowId /: returnPosition)) match {
        case ForkJoinExecutor ⇒ None
        case _: EventInstructionExecutor ⇒ Some(returnPosition)
        case _: PositionInstructionExecutor ⇒ Some(returnPosition.increment)  // Skip IfErrorCode (don't execute again!)
        case _ ⇒ None
      }
    } yield next
}
