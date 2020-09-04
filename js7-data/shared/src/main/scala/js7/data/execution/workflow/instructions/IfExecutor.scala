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

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: If) =
    if (order.isState[Order.Broken] || order.isState[Order.FailedWhileFresh] || order.isState[Order.Failed])
      Right(None)
    else
      nextPosition(context, order, instruction)
        .map(_ map (o => order.id <-: OrderMoved(o)))

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: If) = {
    assertThat(Right(order) == context.idToOrder(order.id).map(_ withPosition order.position))
    context.makeScope(order).evalBoolean(instruction.predicate)
      .map {
        case true => Some(Then)
        case false => instruction.elseWorkflow.isDefined ? Else
      }
      .map {
        case Some(thenOrElse) => Some(order.position / thenOrElse % 0)
        case None => Some(order.position.increment)  // No else-part, skip instruction
      }
  }
}
