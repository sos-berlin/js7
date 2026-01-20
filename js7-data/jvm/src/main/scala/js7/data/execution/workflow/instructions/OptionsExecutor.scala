package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.EngineState
import js7.data.workflow.instructions.Options
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

/**
 * The Options instruction is purely syntactic, like if-then-else.
 * <p>
 *   Because it may be leaved like if-then-else, it must not change state.
 */
private[instructions] final class OptionsExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Options
  val instructionClass = classOf[Options]

  def toEvents(instr: Options, order: Order[Order.State], state: EngineState) =
    Right:
      if !order.isState[IsFreshOrReady] then
        Nil
      else
        (order.id <-: OrderMoved(order.position / BranchId.Options % 0)) :: Nil

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
