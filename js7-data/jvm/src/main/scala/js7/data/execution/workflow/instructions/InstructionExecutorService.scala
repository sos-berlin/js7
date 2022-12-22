package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SubclassToX
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.Instruction

final class InstructionExecutorService(val clock: WallClock)
{
  val finishExecutor = new FinishExecutor(this)

  private val classToExecutor = new SubclassToX(
    Seq(
      new EmptyExecutor(this),
      new EndExecutor(this),
      new ExecuteExecutor(this),
      new FailExecutor(this),
      finishExecutor,
      new ForkExecutor(this),
      new ForkListExecutor(this),
      new GapExecutor(this),
      new IfExecutor(this),
      new TryExecutor(this),
      new LockExecutor(this),
      new PostNoticesExecutor(this),
      new ExpectNoticesExecutor(this),
      new ConsumeNoticesExecutor(this),
      new PromptExecutor(this),
      new RetryExecutor(this),
      new AddOrderExecutor(this),
      new StickySubagentExecutor(this),
      new CycleExecutor(this)
    ).toKeyedMap(_.instructionClass: Class[? <: Instruction]))

  private[instructions] val forkCache = new ForkInstructionExecutor.Cache

  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    classToExecutor.checked(instr.getClass).orThrow

  def nextMove(instruction: Instruction, order: Order[Order.State], stateView: StateView)
  : Checked[Option[OrderMoved]] =
    instructionToExecutor(instruction) match {
      case executor: PositionInstructionExecutor =>
        executor.nextMove(instruction.asInstanceOf[executor.Instr], order, stateView)
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

  def onReturnFromSubworkflow(instr: Instruction, order: Order[Order.State], state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] = {
    val executor = instructionToExecutor(instr)
    executor.onReturnFromSubworkflow(instr.asInstanceOf[executor.Instr], order, state)
  }

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    instructionToExecutor(calculator.stateView.instruction(order.workflowPosition))
      .toObstacles(order, calculator)
}
