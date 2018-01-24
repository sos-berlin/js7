package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFinished}
import com.sos.jobscheduler.data.workflow.{EventInstruction, OrderContext, PositionInstruction}

/**
  * @author Joacim Zschimmer
  */
sealed trait End extends EventInstruction with PositionInstruction
{
  def nextPosition(order: Order[Order.Processed], context: OrderContext) =
    for {
      returnPosition ← order.position.dropChild
      next ← context.instruction(order.workflowPath /: returnPosition) match {
        case _: ForkJoin ⇒ None
        case _: EventInstruction ⇒ Some(returnPosition)
        case _: PositionInstruction ⇒ Some(returnPosition.increment)  // Skip IfErrorCode (don't execute again!)
        case _ ⇒ None
      }
    } yield next

  def toEvent(order: Order[Order.State], context: OrderContext) =
    order.position.dropChild match {
      case None ⇒
        for (order ← order.ifState[Order.Ready]) yield
          if (order.isAttachedToAgent)
            order.id <-: OrderDetachable
          else
            order.id <-: OrderFinished

      case Some(returnPosition) ⇒
        context.instruction(order.workflowPath /: returnPosition) match {
          case _: ForkJoin ⇒
            //if (order.attachedToAgent forall forkjoin.isJoinableOnAgent)
            if (order.isAttachedToAgent)
              Some(order.id <-: OrderDetachable)
            else
              for {
                parentOrderId ← order.parent
                parentOrder ← context.idToOrder.lift(parentOrderId)
                forkJoin ← Some(context.instruction(parentOrder.workflowPosition)) collect { case o: ForkJoin ⇒ o }
                event ← forkJoin.toEvent(parentOrder, context)
              } yield event
          case _ ⇒ None
        }
    }
}

case object ExplicitEnd extends End {
  override def toString = "end"
}

case object ImplicitEnd extends End {
  override def toString = "end/*implicit*/"
}
