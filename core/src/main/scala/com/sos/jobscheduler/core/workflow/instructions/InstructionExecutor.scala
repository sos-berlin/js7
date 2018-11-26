package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, End, Execute, ForkJoin, Gap, If, Offer}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Instruction, OrderContext}

/**
  * @author Joacim Zschimmer
  */
trait InstructionExecutor {
  type Instr <: Instruction
}

trait EventInstructionExecutor extends InstructionExecutor {
  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Instr): Option[KeyedEvent[OrderActorEvent]]
}

trait PositionInstructionExecutor extends InstructionExecutor {
  def nextPosition(context: OrderContext, order: Order[Order.Processed], instruction: Instr): Option[Position]
}

object InstructionExecutor
{
  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    instr match {
      case _: AwaitOrder ⇒ AwaitOrderExecutor
      case _: End ⇒ EndExecutor
      case _: Execute ⇒ ExecuteExecutor
      case _: ForkJoin ⇒ ForkJoinExecutor
      case _: Gap ⇒ GapExecutor
      case _: If ⇒ IfExecutor
      case _: Offer ⇒ OfferExecutor
    }

  def nextPosition(context: OrderContext, order: Order[Order.Processed], instruction: Instruction): Option[Position] =
    instructionToExecutor(instruction) match {
      case exec: PositionInstructionExecutor ⇒ exec.nextPosition(context, order, instruction.asInstanceOf[exec.Instr])
      case _ ⇒ None
    }

  def toEvent(instruction: Instruction, order: Order[Order.State], context: OrderContext): Option[KeyedEvent[OrderActorEvent]] =
    instructionToExecutor(instruction) match {
      case exec: EventInstructionExecutor ⇒
        exec.toEvent(context, order, instruction.asInstanceOf[exec.Instr])
      case _ ⇒
        None
    }

  private[instructions] def ifProcessedThenOrderMoved(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Processed].map(order ⇒
      order.id <-: OrderMoved(order.position.increment))
}
