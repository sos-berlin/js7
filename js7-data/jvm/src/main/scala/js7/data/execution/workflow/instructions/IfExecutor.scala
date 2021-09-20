package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax._
import js7.data.order.Order
import js7.data.order.Order.{Broken, Failed, FailedWhileFresh}
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.StateView
import js7.data.workflow.instructions.If
import js7.data.workflow.position.BranchId.{Else, Then}

private[instructions] final class IfExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = If

  def toEvents(instruction: If, order: Order[Order.State], state: StateView) =
    if (order.isState[Broken] || order.isState[FailedWhileFresh] || order.isState[Failed] || order.isState[Failed])
      Right(Nil)
    else
      nextPosition(instruction, order, state)
        .map(_.map(o => order.id <-: OrderMoved(o)))
        .map(_.toList)

  def nextPosition(instruction: If, order: Order[Order.State], state: StateView) = {
    // order may be predicted and different from actual idToOrder(order.id)
    for {
      scope <- state.toPureScope(order)
      condition <- instruction.predicate.evalAsBoolean(scope)
    } yield
      Some(
        condition ? Then orElse instruction.elseWorkflow.isDefined ? Else match {
          case Some(thenOrElse) => order.position / thenOrElse % 0
          case None => order.position.increment  // No else-part, skip instruction
        })
    }
}
