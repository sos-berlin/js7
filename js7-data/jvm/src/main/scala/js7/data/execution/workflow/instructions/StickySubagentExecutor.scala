package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderStickySubagentEntered, OrderStickySubagentLeaved}
import js7.data.state.StateView
import js7.data.subagent.{SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.instructions.StickySubagent

private[instructions] final class StickySubagentExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = StickySubagent
  val instructionClass = classOf[StickySubagent]

  def toEvents(instr: StickySubagent, order: Order[Order.State], state: StateView) =
    if !order.isState[IsFreshOrReady] then
      Right(Nil)
    else if order.stickySubagents.nonEmpty then
      Left(Problem.pure("StickySubagent instruction must not be nested"))
    else
      instr.subagentSelectionIdExpr
        .fold_(
          Checked(None),
          expr => state
            .toPureOrderScope(order)
            .flatMap(expr.evalAsString(_))
            .flatMap(SubagentSelectionId.checked(_))
            .flatMap(subagentSelectionId => state
              // Check existence
              .keyToItem(SubagentItem).checked(subagentSelectionId.toSubagentId)
              .orElse(state.keyToItem(SubagentSelection).checked(subagentSelectionId))
              .rightAs(Some(subagentSelectionId))))
        .map: subagentSelectionId =>
          (order.id <-: OrderStickySubagentEntered(instr.agentPath, subagentSelectionId))
            :: Nil

  override def onReturnFromSubworkflow(
    instr: StickySubagent,
    order: Order[Order.State],
    state: StateView) =
    Right(
      (order.id <-: OrderStickySubagentLeaved) :: Nil)
