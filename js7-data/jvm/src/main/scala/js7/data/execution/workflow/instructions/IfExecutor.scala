package js7.data.execution.workflow.instructions

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.CatsUtils.syntax.view
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.moveOrder
import js7.data.execution.workflow.instructions.IfExecutor.*
import js7.data.order.Order
import js7.data.order.Order.{Broken, Failed, FailedWhileFresh}
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderMoved, OrderStarted}
import js7.data.order.{Order, OrderId}
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.instructions.If
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

private object IfExecutor extends EventInstructionExecutor_[If], PositionInstructionExecutor:

  private val logger = Logger[this.type]

  def toEventCalc[S <: EngineState_[S]](instr: If, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      if order.isState[Broken] || order.isState[FailedWhileFresh] || order.isState[Failed] then
        coll.nix
      else
        try
          nextMove(instr, order, coll.aggregate).flatMap:
            case None =>
              coll.nix
            case Some(OrderMoved(to, reason)) =>
              coll:
                moveOrder(order, to, reason)
        catch case _: OrderNotStartedException =>
          if !order.isStartable(coll.now) then
            logger.trace(s"🪱 Useless execution of clock-querying if-instruction when ${
              order.id
            } has not reached its scheduled time")
            coll.nix
          else
            coll:
              orderId <-: OrderStarted

  def nextMove(instruction: If, order: Order[Order.State], state: EngineState) =
    // order may be predicted and different from actual idToOrder(order.id)
    try
      myNextMove(instruction, order, state)
    catch case _: OrderNotStartedException =>
      Right(None)

  private def myNextMove(instruction: If, order: Order[Order.State], state: EngineState) =
    // order may be predicted and different from actual idToOrder(order.id)
    for
      scope <- state.toImpureOrderExecutingScope(
        order,
        if order.isStarted then
          clock.now()
        else
          new OrderNotStartedException/*catched below*/)
      branchId <-
        instruction.ifThens.view
          .scanLeft(Checked(false) -> 0):
            case ((_, i), ifThen) => ifThen.predicate.evalAsBoolean(using scope) -> (i + 1)
          .appended:
            Right(true) -> instruction.elseBlock.fold(-1)(_ => 0)
          .takeThrough: o => // Skip false predicates
            o._1.contains(false)
          .last
          .match
            case (checked, -1) => checked.rightAs(None) // No then, no else
            case (checked, 0) => checked.rightAs(Some(BranchId.Else))
            case (checked, i) => checked.rightAs(Some(BranchId.then_(i)))
    yield Some:
      OrderMoved:
        branchId match
          case Some(branchId) => order.position / branchId % 0
          case None => order.position.increment  // No else-part, skip instruction

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)


  private final class OrderNotStartedException extends RuntimeException
