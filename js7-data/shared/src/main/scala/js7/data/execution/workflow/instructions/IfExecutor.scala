package js7.data.execution.workflow.instructions

import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.data.execution.workflow.context.OrderContext
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

  def toEvent(instruction: If, order: Order[Order.State], context: OrderContext) =
    if (order.isState[Order.Broken] || order.isState[Order.FailedWhileFresh] || order.isState[Order.Failed])
      Right(None)
    else
      nextPosition(instruction, order, context)
        .map(_.map(o => order.id <-: OrderMoved(o)))

  def nextPosition(instruction: If, order: Order[Order.State], context: OrderContext) = {
    assertThat(Right(order) == context.idToOrder(order.id).map(_ withPosition order.position))
    context.makeScope(order)
      .flatMap(_.evalBoolean(instruction.predicate))
      .map { condition =>
        Some(
          condition ? Then orElse instruction.elseWorkflow.isDefined ? Else match {
            case Some(thenOrElse) => order.position / thenOrElse % 0
            case None => order.position.increment  // No else-part, skip instruction
          })
      }
  }
}
