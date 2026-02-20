package js7.data.execution.workflow.instructions

import js7.base.time.Timestamp
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.OrderEventSource.moveOrder
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderMoved}
import js7.data.order.{Order, OrderId}
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchId.try_
import js7.data.workflow.position.BranchPath.syntax.*

private object TryExecutor extends
  EventInstructionExecutor_[TryInstruction],
  PositionInstructionExecutor:

  def toEventCalc[S <: EngineState_[S]](instr: TryInstruction, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      coll:
        order.ifState[Order.IsFreshOrReady].map: order =>
          moveOrder(order, nextPosition(order))

  override def nextMove(
    instr: TryInstruction,
    order: Order[Order.State],
    state: EngineState,
    now: Timestamp) =
    Right(Some(nextOrderMoved(order)))

  private def nextOrderMoved(order: Order[Order.State]) =
    OrderMoved(nextPosition(order))

  private def nextPosition(order: Order[Order.State]) =
    order.position / try_(0) % 0

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
