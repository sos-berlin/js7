package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{AwaitOrder, End, Execute, Fail, Finish, Fork, Gap, If, LockInstruction, Offer, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
trait InstructionExecutor {
  type Instr <: Instruction
}

trait EventInstructionExecutor extends InstructionExecutor
{
  def toEvents(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]]
}

trait PositionInstructionExecutor extends InstructionExecutor
{
  def nextPosition(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[Option[Position]]
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
      case _: LockInstruction => LockExecutor
      case _: Offer => OfferExecutor
      case _: Prompt => PromptExecutor
      case _: Retry => new RetryExecutor(() => Timestamp.now)
    }

  def nextPosition(instruction: Instruction, order: Order[Order.State], stateView: StateView)
  : Checked[Option[Position]] =
    instructionToExecutor(instruction) match {
      case executor: PositionInstructionExecutor =>
        executor.nextPosition(instruction.asInstanceOf[executor.Instr], order, stateView)
      case _ => Right(None)
    }

  def toEvents(instruction: Instruction, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    instructionToExecutor(instruction) match {
      case exec: EventInstructionExecutor =>
        exec.toEvents(instruction.asInstanceOf[exec.Instr], order, stateView)
      case _ =>
        Right(Nil)
    }

  private[instructions] def ifProcessedThenOrderMoved(order: Order[Order.State], stateView: StateView) =
    order.ifState[Order.Processed].map(order =>
      order.id <-: OrderMoved(order.position.increment))
}
