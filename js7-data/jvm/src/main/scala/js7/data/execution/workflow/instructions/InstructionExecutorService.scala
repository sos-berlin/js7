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
import org.jetbrains.annotations.TestOnly

final class InstructionExecutorService(val clock: WallClock):

  val finishExecutor = new FinishExecutor(this)

  private val classToExecutor = new SubclassToX(
    Seq(
      EmptyExecutor(this),
      EndExecutor(this),
      ExecuteExecutor(this),
      FailExecutor(this),
      finishExecutor,
      ForkExecutor(this),
      ForkListExecutor(this),
      GapExecutor(this),
      IfExecutor(this),
      TryExecutor(this),
      LockExecutor(this),
      PostNoticesExecutor(this),
      ExpectNoticesExecutor(this),
      ConsumeNoticesExecutor(this),
      PromptExecutor(this),
      RetryExecutor(this),
      AddOrderExecutor(this),
      StickySubagentExecutor(this),
      OptionsExecutor(this),
      StopExecutor(this),
      BreakOrderExecutor(this),
      CycleExecutor(this),
      SleepExecutor(this),
      SayExecutor(this),
      BreakExecutor(this)
    ).toKeyedMap(_.instructionClass: Class[? <: Instruction]))

  private[instructions] def instructionToExecutor(instr: Instruction): InstructionExecutor =
    classToExecutor.checked(instr.getClass).orThrow

  def nextMove(instruction: Instruction, order: Order[Order.State], stateView: StateView)
  : Checked[Option[OrderMoved]] =
    instructionToExecutor(instruction) match
      case executor: PositionInstructionExecutor =>
        executor.nextMove(instruction.asInstanceOf[executor.Instr], order, stateView)
      case _ => Right(None)

  @TestOnly
  def toEvents(orderId: OrderId, stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    for
      order <- stateView.idToOrder.checked(orderId)
      events <- toEvents(stateView.instruction(order.workflowPosition), order, stateView)
    yield events

  @TestOnly
  def toEvents(order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    toEvents(stateView.instruction(order.workflowPosition), order, stateView)

  def toEvents(instruction: Instruction, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    instructionToExecutor(instruction) match
      case exec: EventInstructionExecutor =>
        exec.toEvents(instruction.asInstanceOf[exec.Instr], order, stateView)
      case _ =>
        Right(Nil)

  def onReturnFromSubworkflow(instr: Instruction, order: Order[Order.State], state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    val executor = instructionToExecutor(instr)
    executor.onReturnFromSubworkflow(instr.asInstanceOf[executor.Instr], order, state)

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    instructionToExecutor(calculator.stateView.instruction(order.workflowPosition))
      .toObstacles(order, calculator)
