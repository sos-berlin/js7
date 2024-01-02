package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.InstructionExecutor.*
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachable, OrderDetachable, OrderMoved, OrderStarted}
import js7.data.order.{Order, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.Instruction
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
trait InstructionExecutor:
  type Instr <: Instruction

  protected val service: InstructionExecutorService

  def instructionClass: Class[? <: Instr]

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    noObstacles

  def onReturnFromSubworkflow(instr: Instr, order: Order[Order.State], state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    order.position.parent match
      case None =>
        // Does not happen
        Left(Problem.pure(s"onReturnFromSubworkflow but no parent position for ${order.id}"))

      case Some(parentPos) =>
        Right(
          (order.id <-: OrderMoved(parentPos.increment)) :: Nil)

  def subworkflowEndToPosition(parentPos: Position): Option[Position] =
    None


object InstructionExecutor:
  private def noObstacles: Checked[Set[OrderObstacle]] =
    Right(Set.empty)

trait EventInstructionExecutor extends InstructionExecutor:
  final def clock: WallClock =
    service.clock

  def toEvents(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]]

  protected final def start(order: Order[Order.State])
  : Option[Checked[List[KeyedEvent[OrderStarted]]]] =
    for maybeStartable <- maybeStart(order) yield
      Right(maybeStartable.map(order.id <-: _).toList)

  private def maybeStart(order: Order[Order.State]): Option[Option[OrderStarted]] =
    order.isState[Order.Fresh] ?
      (isNotDelayed(order) ?
        OrderStarted)

  protected final def readyOrStartable(order: Order[Order.State])
  : Option[Order[Order.IsFreshOrReady]] =
    order
      .ifState[Order.Ready]
      .orElse(order
        .ifState[Order.Fresh]
        .filter(isNotDelayed))

  protected final def isNotDelayed(order: Order[Order.State]): Boolean =
    order.maybeDelayedUntil.forall(_ <= clock.now())

  protected final def attach(order: Order[Order.State], agentPath: AgentPath)
  : Option[Checked[List[KeyedEvent[OrderAttachable]]]] =
    order.isDetached ? Right((order.id <-: OrderAttachable(agentPath)) :: Nil)

  protected final def detach(order: Order[Order.State])
  : Option[Checked[List[KeyedEvent[OrderDetachable]]]] =
    order.isAttached ? Right((order.id <-: OrderDetachable) :: Nil)

trait PositionInstructionExecutor extends InstructionExecutor:
  def nextMove(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[Option[OrderMoved]]
