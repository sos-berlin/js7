package js7.data.execution.workflow.instructions

import js7.base.log.Logger
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.leaveBlocks
import js7.data.execution.workflow.instructions.BreakExecutor.*
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderCycleFinished, OrderFinished}
import js7.data.order.OrderId
import js7.data.state.EngineEventColl.extensions.workflow
import js7.data.state.EngineStateExtensions.atController
import js7.data.state.EngineState_
import js7.data.workflow.instructions.Break

private object BreakExecutor extends EventInstructionExecutor_[Break]:
  private val logger = Logger[this.type]

  def toEventCalc[S <: EngineState_[S]](instr: Break, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      start(coll, orderId): (coll, order) =>
        for
          workflow <- coll.workflow(order.workflowId)
          coll <- coll.addEventCalc:
            leaveBlocks(
              orderId, workflow,
              until = _.isCycle,
              events =
                case Some(branchId) if branchId.isCycle =>
                  EventCalc.pure:
                    orderId <-: OrderCycleFinished(None)

                case None =>
                  EventCalc: coll =>
                    coll:
                      coll.aggregate.atController(orderId):
                        OrderFinished()

                case Some(branchId) =>
                  // Just in case
                  logger.warn:
                    s"Unexpected BranchId:$branchId encountered. ${order.id} position=${order.position}"
                  // Best guess, to avoid a crash: TODO Let the order fail with uncatchable
                  EventCalc: coll =>
                    coll:
                      coll.aggregate.atController(orderId):
                        OrderFinished())
        yield coll
