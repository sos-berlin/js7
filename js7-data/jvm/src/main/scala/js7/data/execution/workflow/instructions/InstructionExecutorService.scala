package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.state.StateView
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{AddOrder, End, Execute, ExpectNotice, Fail, Finish, Fork, ForkList, Gap, If, LockInstruction, PostNotice, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.Position

final class InstructionExecutorService(val clock: WallClock)
{
  private val endExecutor = new EndExecutor(this)
  private val executeExecutor = new ExecuteExecutor(this)
  private val failExecutor = new FailExecutor(this)
  private val finishExecutor = new FinishExecutor(this)
  private[workflow] val forkExecutor = new ForkExecutor(this)
  private val forkListExecutor = new ForkListExecutor(this)
  private val gapExecutor = new GapExecutor(this)
  private val ifExecutor = new IfExecutor(this)
  private val tryExecutor = new TryExecutor(this)
  private[instructions] val lockExecutor = new LockExecutor(this)
  private val postNoticeExecutor = new PostNoticeExecutor(this)
  private val expectNoticeExecutor = new ExpectNoticeExecutor(this)
  private val promptExecutor = new PromptExecutor(this)
  private val retryExecutor = new RetryExecutor(this)
  private val addOrderExecutor = new AddOrderExecutor(this)

  private[instructions] val forkCache = new ForkInstructionExecutor.Cache

  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    instr match {
      case _: End => endExecutor
      case _: Execute => executeExecutor
      case _: Fail => failExecutor
      case _: Finish => finishExecutor
      case _: Fork => forkExecutor
      case _: ForkList => forkListExecutor
      case _: Gap => gapExecutor
      case _: If => ifExecutor
      case _: TryInstruction => tryExecutor
      case _: LockInstruction => lockExecutor
      case _: PostNotice => postNoticeExecutor
      case _: ExpectNotice => expectNoticeExecutor
      case _: Prompt => promptExecutor
      case _: Retry => retryExecutor
      case _: AddOrder => addOrderExecutor
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

  object implicits {
    implicit val defaultInstructionExecutorService = default
  }
}
