package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventCalc
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderStickySubagentEntered, OrderStickySubagentLeaved}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.state.EngineState_
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentItem}
import js7.data.workflow.instructions.StickySubagent

object StickySubagentExecutor extends EventInstructionExecutor_[StickySubagent]:

  def toEventCalc[S <: EngineState_[S]](instr: StickySubagent, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      if !order.isState[IsFreshOrReady] then
        coll.nix
      else if order.stickySubagents.nonEmpty then
        Left(Problem.pure("StickySubagent instruction must not be nested"))
      else
        instr.subagentBundleIdExpr.fold(Right(None)): expr =>
          coll.aggregate.toOrderScope(order)
            .flatMap:
              expr.evalAsString(using _)
            .flatMap(SubagentBundleId.checked(_))
            .flatMap: subagentBundleId =>
              // Check existence
              coll.aggregate.keyToItem(SubagentItem).checked(subagentBundleId.toSubagentId)
                .orElse:
                  coll.aggregate.keyToItem(SubagentBundle).checked(subagentBundleId)
                .rightAs:
                  Some(subagentBundleId)
        .flatMap: subagentBundleId =>
          coll:
            order.id <-: OrderStickySubagentEntered(instr.agentPath, subagentBundleId)

  override def onReturnFromSubworkflow[S <: EngineState_[S]](instr: Instr, order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc.pure:
      order.id <-: OrderStickySubagentLeaved
