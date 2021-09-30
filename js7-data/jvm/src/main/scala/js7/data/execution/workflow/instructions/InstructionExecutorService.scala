package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SubclassToX
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.state.StateView
import js7.data.workflow.Instruction
import js7.data.workflow.position.Position

final class InstructionExecutorService(val clock: WallClock)
{
  private[workflow] val forkExecutor = new ForkExecutor(this)
  private[instructions] val lockExecutor = new LockExecutor(this)

  private val classToExecutor = new SubclassToX(
    Seq(
      new EndExecutor(this),
      new ExecuteExecutor(this),
      new FailExecutor(this),
      new FinishExecutor(this),
      forkExecutor,
      new ForkListExecutor(this),
      new GapExecutor(this),
      new IfExecutor(this),
      new TryExecutor(this),
      lockExecutor,
      new PostNoticeExecutor(this),
      new ExpectNoticeExecutor(this),
      new PromptExecutor(this),
      new RetryExecutor(this),
      new AddOrderExecutor(this)
    ).toKeyedMap(_.instructionClass: Class[_ <: Instruction]))

  private[instructions] val forkCache = new ForkInstructionExecutor.Cache

  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    classToExecutor.checked(instr.getClass).orThrow

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
