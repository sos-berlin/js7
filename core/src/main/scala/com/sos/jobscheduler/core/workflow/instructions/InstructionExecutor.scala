package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, End, Execute, Fail, Fork, Gap, If, Offer, Retry, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.Position

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
  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: Instr): Option[Position]
}

object InstructionExecutor
{
  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    instr match {
      case _: AwaitOrder => AwaitOrderExecutor
      case _: End => EndExecutor
      case _: Execute => ExecuteExecutor
      case _: Fail => FailExecutor
      case _: Fork => ForkExecutor
      case _: Gap => GapExecutor
      case _: If => IfExecutor
      case _: TryInstruction => TryExecutor
      case _: Offer => OfferExecutor
      case _: Retry => new RetryExecutor(() => now)
    }

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: Instruction): Option[Position] =
    instructionToExecutor(instruction) match {
      case exec: PositionInstructionExecutor => exec.nextPosition(context, order, instruction.asInstanceOf[exec.Instr])
      case _ => None
    }

  def toEvent(instruction: Instruction, order: Order[Order.State], context: OrderContext): Option[KeyedEvent[OrderActorEvent]] =
    instructionToExecutor(instruction) match {
      case exec: EventInstructionExecutor =>
        exec.toEvent(context, order, instruction.asInstanceOf[exec.Instr])
      case _ =>
        None
    }

  private[instructions] def ifProcessedThenOrderMoved(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Processed].map(order =>
      order.id <-: OrderMoved(order.position.increment))
}
