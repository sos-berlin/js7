package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.{Timestamp, WallClock}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{End, Execute, Fail, Finish, Fork, Gap, If, LockInstruction, PostNotice, Prompt, ReadNotice, Retry, TryInstruction}
import js7.data.workflow.position.Position

final class InstructionExecutorService(clock: WallClock)
{
  private val postNoticeExecutor = new PostNoticeExecutor(clock)

  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    instr match {
      case _: End => new EndExecutor(this)
      case _: Execute => ExecuteExecutor
      case _: Fail => FailExecutor
      case _: Finish => FinishExecutor
      case _: Fork => ForkExecutor
      case _: Gap => GapExecutor
      case _: If => IfExecutor
      case _: TryInstruction => TryExecutor
      case _: LockInstruction => LockExecutor
      case _: PostNotice => postNoticeExecutor
      case _: ReadNotice => ReadNoticeExecutor
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
}

object InstructionExecutorService
{
  val default = new InstructionExecutorService(WallClock)

  implicit val defaultInstructionExecutorService = default
}