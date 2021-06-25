package js7.data.execution.workflow.instructions

import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderMoved
import js7.data.workflow.instructions.If
import js7.data.workflow.position.BranchId.{Else, Then}

/**
  * @author Joacim Zschimmer
  */
object IfExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = If

  def toEvents(instruction: If, order: Order[Order.State], state: StateView) =
    if (order.isState[Order.Broken] || order.isState[Order.FailedWhileFresh] || order.isState[Order.Failed])
      Right(Nil)
    else
      nextPosition(instruction, order, state)
        .map(_.map(o => order.id <-: OrderMoved(o)))
        .map(_.toList)

  def nextPosition(instruction: If, order: Order[Order.State], state: StateView) = {
    assertThat(Right(order) == state.idToOrder(order.id).map(_ withPosition order.position))
    for {
      scope <- state.toScope(order)
      condition <- instruction.predicate.evalAsBoolean(scope)
    } yield
      Some(
        condition ? Then orElse instruction.elseWorkflow.isDefined ? Else match {
          case Some(thenOrElse) => order.position / thenOrElse % 0
          case None => order.position.increment  // No else-part, skip instruction
        })
    }
}
