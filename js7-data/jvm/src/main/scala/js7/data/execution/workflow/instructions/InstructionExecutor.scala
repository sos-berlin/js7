package js7.data.execution.workflow.instructions

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SubclassToX
import js7.data.event.{EventCalc, EventColl, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.OrderEventSource.moveOrder
import js7.data.execution.workflow.instructions.InstructionExecutor.*
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderMoved}
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.EngineEventColl.extensions.order
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.Instruction
import js7.data.workflow.position.Position
import org.jetbrains.annotations.TestOnly

trait InstructionExecutor:
  type Instr <: Instruction

  def instructionClass: Class[? <: Instr]

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator, now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    noObstacles

  def onReturnFromSubworkflow[S <: EngineState_[S]](instr: Instr, order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    order.position.parent match
      case None =>
        // Does not happen
        EventCalc.problem:
          Problem.pure(s"onReturnFromSubworkflow but no parent position for ${order.id}")

      case Some(parentPos) =>
        moveOrder(order, parentPos.increment).widen

  def subworkflowEndToPosition(parentPos: Position): Option[Position] =
    None


object InstructionExecutor:
  private val logger = Logger[this.type]

  private val classToExecutor =
    SubclassToX.by[Instruction, InstructionExecutor](_.instructionClass,
      EmptyExecutor,
      EndExecutor,
      ExecuteExecutor,
      FailExecutor,
      FinishExecutor,
      ForkExecutor,
      ForkListExecutor,
      GapExecutor,
      IfExecutor,
      TryExecutor,
      LockExecutor,
      PostNoticesExecutor,
      ExpectNoticesExecutor,
      ConsumeNoticesExecutor,
      PromptExecutor,
      RetryExecutor,
      AddOrderExecutor,
      StickySubagentExecutor,
      OptionsExecutor,
      StopExecutor,
      BreakOrderExecutor,
      CycleExecutor,
      AdmissionTimeExecutor,
      SleepExecutor,
      SayExecutor,
      BreakExecutor)

  private val noObstacles: Checked[Set[OrderObstacle]] =
    Right(Set.empty)

  private[instructions] inline def apply(instr: Instruction): InstructionExecutor =
    byInstr(instr)

  private def byInstr(instr: Instruction): InstructionExecutor =
    classToExecutor.checked(instr.getClass).orThrow

  def nextMove(order: Order[Order.State], engineState: EngineState): Checked[Option[OrderMoved]] =
    engineState.instruction(order.workflowPosition).flatMap: instr =>
      byInstr(instr) match
        case executor: PositionInstructionExecutor =>
          executor.nextMove(instr.asInstanceOf[executor.Instr], order, engineState)
        case _ => Right(None)

  @TestOnly
  def testToEvents[S <: EngineState_[S]](
    orderId: OrderId,
    engineState: S,
    now: Timestamp = Timestamp("2099-01-01T00:00:00Z"))
  : Checked[List[KeyedEvent[OrderCoreEvent]]] =
    toEventCalc(orderId)
      .calculateEvents(EventColl(engineState, now))
      .map(_.toList)

  @TestOnly
  def toEventCalc[S <: EngineState_[S]](orderId: OrderId): EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll.order(orderId).flatMap: order =>
        coll:
          toEventCalc[S](order)

  def toEventCalc[S <: EngineState_[S]](order: Order[Order.State]): EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      val orderId = order.id
      for
        instr <- coll.aggregate.instruction(order.workflowPosition)
        coll <- coll:
          byInstr(instr) match
            case executor: EventInstructionExecutor =>
              executor.toEventCalc(instr.asInstanceOf[executor.Instr], orderId)
            case _ =>
              EventCalc.empty
      yield
        if !coll.hasEvents then
          logger.trace(s"🪱 ${instr.getClass.shortClassName} returned no event for $order")
        coll

  def onReturnFromSubworkflow[S <: EngineState_[S]](
    instr: Instruction,
    order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    val executor = byInstr(instr)
    executor.onReturnFromSubworkflow(instr.asInstanceOf[executor.Instr], order)

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator, now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    calculator.engineState.instruction(order.workflowPosition).flatMap: instr =>
      byInstr(instr).toObstacles(order, calculator, now)
