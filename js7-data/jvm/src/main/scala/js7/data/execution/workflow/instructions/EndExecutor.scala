package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.event.EventCalc
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderFinished, OrderMoved}
import js7.data.order.{Order, OrderId}
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.instructions.End

private object EndExecutor extends EventInstructionExecutor_[End], PositionInstructionExecutor:

  def toEventCalc[S <: EngineState_[S]](instr: End, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      order.position.parent
        .filter(_ => !order.isInOutermostBlock)
        .match
          case None =>
            order.state match
              case _: Order.IsFreshOrReady =>
                detach(coll, order): coll =>
                  start(coll, orderId): (coll, order) =>
                    coll:
                      orderId <-: OrderFinished()

              case _ => coll.nix

          case Some(parentPos) =>
            coll.aggregate.instruction(order.workflowId /: parentPos).flatMap: instr =>
              coll:
                InstructionExecutor.onReturnFromSubworkflow(instr, order)

  def nextMove(
    instr: Instr,
    order: Order[Order.State],
    engineState: EngineState,
    now: Timestamp)
  : Checked[Option[OrderMoved]] =
    if order.isInOutermostBlock then
      Right(None)
    else
      order.position.parent.flatMap: parentPos =>
        engineState.instruction(order.workflowId /: parentPos).traverse: instr =>
          InstructionExecutor(instr).subworkflowEndToPosition(parentPos).map: next =>
            OrderMoved(next)
      .sequence
