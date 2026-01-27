package js7.data.execution.workflow.instructions

import js7.data.event.EventCalc
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderNoticesConsumed}
import js7.data.state.EngineStateExtensions.atController
import js7.data.state.EngineState_
import js7.data.workflow.instructions.ConsumeNotices

private[instructions] object ConsumeNoticesExecutor extends ConsumeOrExpectNoticesExecutor:

  type Instr = ConsumeNotices
  val instructionClass = classOf[ConsumeNotices]

  override final def onReturnFromSubworkflow[S <: EngineState_[S]](
    instr: ConsumeNotices,
    order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll:
       coll.aggregate.atController(order.id):
          OrderNoticesConsumed()
