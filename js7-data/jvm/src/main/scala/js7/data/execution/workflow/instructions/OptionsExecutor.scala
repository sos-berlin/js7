package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.moveOrderDownToBranch
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.OrderCoreEvent
import js7.data.order.OrderId
import js7.data.state.EngineState_
import js7.data.workflow.instructions.Options
import js7.data.workflow.position.*

/**
  * The Options instruction is purely syntactic, like if-then-else.
  * <p>
  * Because it may be leaved like if-then-else, it must not change state.
  */
object OptionsExecutor extends EventInstructionExecutor_[Options]:

  def toEventCalc[S <: EngineState_[S]](instr: Options, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      if order.isState[IsFreshOrReady] then
        coll:
          moveOrderDownToBranch(order, BranchId.Options)
      else
        coll.nix

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
