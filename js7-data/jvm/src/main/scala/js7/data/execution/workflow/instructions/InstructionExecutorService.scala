package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SubclassToX
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{Fork, ForkInstruction, ForkList}
import js7.data.workflow.position.Position

final class InstructionExecutorService(val clock: WallClock)
{
  val finishExecutor = new FinishExecutor(this)
  private[workflow] val forkExecutor = new ForkExecutor(this)
  private[workflow] val forkListExecutor = new ForkListExecutor(this)
  private[instructions] val lockExecutor = new LockExecutor(this)
  private[instructions] val cycleExecutor = new CycleExecutor(this)

  private val classToExecutor = new SubclassToX(
    Seq(
      new EndExecutor(this),
      new ExecuteExecutor(this),
      new FailExecutor(this),
      finishExecutor,
      forkExecutor,
      forkListExecutor,
      new GapExecutor(this),
      new IfExecutor(this),
      new TryExecutor(this),
      lockExecutor,
      new PostNoticesExecutor(this),
      new ExpectNoticesExecutor(this),
      new PromptExecutor(this),
      new RetryExecutor(this),
      new AddOrderExecutor(this),
      cycleExecutor
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

  def toEvents(orderId: OrderId, stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    for {
      order <- stateView.idToOrder.checked(orderId)
      events <- toEvents(stateView.instruction(order.workflowPosition), order, stateView)
    } yield events

  def toEvents(order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    toEvents(stateView.instruction(order.workflowPosition), order, stateView)

  def toEvents(instruction: Instruction, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    instructionToExecutor(instruction) match {
      case exec: EventInstructionExecutor =>
        exec.toEvents(instruction.asInstanceOf[exec.Instr], order, stateView)
      case _ =>
        Right(Nil)
    }

  private[execution] def tryJoinChildOrder(
    fork: ForkInstruction,
    order: Order[Order.State],
    state: StateView)
  : Option[KeyedEvent[OrderActorEvent]] =
    fork match {
      case fork: Fork => forkExecutor.tryJoinChildOrder(fork, order, state)
      case fork: ForkList => forkListExecutor.tryJoinChildOrder(fork, order, state)
    }

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    instructionToExecutor(calculator.stateView.instruction(order.workflowPosition))
      .toObstacles(order, calculator)
}
