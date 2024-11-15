package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Order
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderStickySubagentEntered, OrderStickySubagentLeaved}
import js7.data.state.StateView
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentItem}
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
      instr.subagentBundleIdExpr
        .fold_(
          Checked(None),
          expr => state
            .toOrderScope(order)
            .flatMap(expr.evalAsString(_))
            .flatMap(SubagentBundleId.checked(_))
            .flatMap(subagentBundleId => state
              // Check existence
              .keyToItem(SubagentItem).checked(subagentBundleId.toSubagentId)
              .orElse(state.keyToItem(SubagentBundle).checked(subagentBundleId))
              .rightAs(Some(subagentBundleId))))
        .map: subagentBundleId =>
          (order.id <-: OrderStickySubagentEntered(instr.agentPath, subagentBundleId))
            :: Nil

  override def onReturnFromSubworkflow(
    instr: StickySubagent,
    order: Order[Order.State],
    state: StateView) =
    Right(
      (order.id <-: OrderStickySubagentLeaved) :: Nil)
