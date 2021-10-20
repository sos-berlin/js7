package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.InstructionExecutor._
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachable, OrderDetachable, OrderStarted}
import js7.data.order.{Order, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.Instruction
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
trait InstructionExecutor
{
  type Instr <: Instruction

  def instructionClass: Class[_ <: Instr]

  def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator)
  : Checked[Set[OrderObstacle]] =
    noObstacles
}

object InstructionExecutor {
  private def noObstacles: Checked[Set[OrderObstacle]] =
    Right(Set.empty)
}

trait EventInstructionExecutor extends InstructionExecutor
{
  protected val service: InstructionExecutorService

  final def clock: WallClock =
    service.clock

  def toEvents(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]]

  protected final def start(order: Order[Order.State])
  : Option[Checked[List[KeyedEvent[OrderStarted]]]] =
    order.isState[Order.Fresh] ?
      Right(order.maybeDelayedUntil.forall(_ <= clock.now())
        .thenList(order.id <-: OrderStarted))

  protected final def attach(order: Order[Order.State], agentPath: AgentPath)
  : Option[Checked[List[KeyedEvent[OrderAttachable]]]] =
    order.isDetached ? Right((order.id <-: OrderAttachable(agentPath)) :: Nil)

  protected final def detach(order: Order[Order.State])
  : Option[Checked[List[KeyedEvent[OrderDetachable]]]] =
    order.isAttached ? Right((order.id <-: OrderDetachable) :: Nil)
}

trait PositionInstructionExecutor extends InstructionExecutor
{
  def nextPosition(instruction: Instr, order: Order[Order.State], stateView: StateView)
  : Checked[Option[Position]]
}
