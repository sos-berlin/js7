package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.fail
import js7.data.order.Order.Ready
import js7.data.order.OrderEvent.OrderCoreEvent
import js7.data.order.{OrderId, OrderOutcome}
import js7.data.state.EngineState_
import js7.data.workflow.instructions.Fail

object FailExecutor extends EventInstructionExecutor_[Fail]:

  def toEventCalc[S <: EngineState_[S]](instr: Fail, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    // Should the order really start ???
    EventCalc: coll =>
      start(coll, orderId): (coll, order) =>
        order.ifState[Ready].map: order =>
          val msg = instr.message.map: messageExpr =>
            coll.aggregate.toImpureOrderExecutingScope(order, coll.now).flatMap: scope =>
              messageExpr.evalAsString(using scope)
            .fold(_.toString, identity)
          coll.add:
            fail[S](orderId, Some:
              OrderOutcome.Failed(msg, instr.namedValues, uncatchable = instr.uncatchable),
              uncatchable = instr.uncatchable)
        .getOrElse:
          coll.nix