package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Order
import js7.data.order.Order.{Broken, Failed, FailedWhileFresh}
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.StateView
import js7.data.workflow.instructions.If
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.*

private[instructions] final class IfExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = If
  val instructionClass = classOf[If]

  def toEvents(instruction: If, order: Order[Order.State], state: StateView) =
    if (order.isState[Broken] || order.isState[FailedWhileFresh] || order.isState[Failed])
      Right(Nil)
    else
      nextMove(instruction, order, state)
        .map(_.map(order.id <-: _))
        .map(_.toList)

  def nextMove(instruction: If, order: Order[Order.State], state: StateView) =
    // order may be predicted and different from actual idToOrder(order.id)
    for {
      scope <- state.toPureOrderScope(order)
      condition <- instruction.predicate.evalAsBoolean(scope)
    } yield
      Some(OrderMoved(
        condition ? Then orElse instruction.elseWorkflow.isDefined ? Else match {
          case Some(thenOrElse) => order.position / thenOrElse % 0
          case None => order.position.increment  // No else-part, skip instruction
        }))

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
}
