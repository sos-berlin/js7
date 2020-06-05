package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.OrderContext
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{AwaitOrder, End, Execute, Fail, Finish, Fork, Gap, If, Offer, Retry, TryInstruction}
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
trait InstructionExecutor {
  type Instr <: Instruction
}

trait EventInstructionExecutor extends InstructionExecutor
{
  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Instr): Checked[Option[KeyedEvent[OrderActorEvent]]]
}

trait PositionInstructionExecutor extends InstructionExecutor
{
  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: Instr): Checked[Option[Position]]
}

object InstructionExecutor
{
  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    instr match {
      case _: AwaitOrder => AwaitOrderExecutor
      case _: End => EndExecutor
      case _: Execute => ExecuteExecutor
      case _: Fail => FailExecutor
      case _: Finish => FinishExecutor
      case _: Fork => ForkExecutor
      case _: Gap => GapExecutor
      case _: If => IfExecutor
      case _: TryInstruction => TryExecutor
      case _: Offer => OfferExecutor
      case _: Retry => new RetryExecutor(() => Timestamp.now)
    }

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: Instruction): Checked[Option[Position]] =
    instructionToExecutor(instruction) match {
      case executor: PositionInstructionExecutor => executor.nextPosition(context, order, instruction.asInstanceOf[executor.Instr])
      case _ => Right(None)
    }

  def toEvent(instruction: Instruction, order: Order[Order.State], context: OrderContext): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    instructionToExecutor(instruction) match {
      case exec: EventInstructionExecutor =>
        exec.toEvent(context, order, instruction.asInstanceOf[exec.Instr])
      case _ =>
        Right(None)
    }

  private[instructions] def ifProcessedThenOrderMoved(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Processed].map(order =>
      order.id <-: OrderMoved(order.position.increment))
}
