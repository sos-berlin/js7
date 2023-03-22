package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.StateView
import js7.data.workflow.instructions.Options
import js7.data.workflow.position.{BranchId, Position}

private[instructions] final class OptionsExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Options
  val instructionClass = classOf[Options]

  def toEvents(instr: Options, order: Order[Order.State], state: StateView) =
    if (!order.isState[IsFreshOrReady])
      Right(Nil)
    else
      Right((order.id <-: OrderMoved(order.position / BranchId.Options % 0)) :: Nil)

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
}
