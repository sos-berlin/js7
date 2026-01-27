package js7.data.execution.workflow.instructions

import js7.base.log.Logger
import js7.data.event.EventCalc
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.state.EngineStateExtensions.atController
import js7.data.state.EngineState_
import js7.data.workflow.instructions.LockInstruction

private object LockExecutor extends EventInstructionExecutor_[LockInstruction]:
  private val logger = Logger[this.type]

  def toEventCalc[S <: EngineState_[S]](instr: LockInstruction, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        if order.isState[Order.Ready] || order.isState[Order.WaitingForLock] then
          coll.aggregate.foreachLockDemand(instr.demands):
            _.isAvailable(order.id, _)
          .flatMap: availability =>
            if availability.forall(identity) then
              coll.aggregate.foreachLockDemand(instr.demands):
                _.acquire(order.id, _) /*check only*/
              .flatMap: _ =>
                coll:
                  orderId <-: OrderLocksAcquired(instr.demands)
            else if order.isState[Order.WaitingForLock] then
              // Caller trys too often ???
              logger.trace(s"🟡 ${order.id} is still WaitingForLock: ${
                instr.demands.zip(availability).collect:
                  case (demand, false) => demand.lockPath
                .mkString(" ")
              }")
              coll.nix
            else
              coll.aggregate.foreachLockDemand(instr.demands):
                _.enqueue(order.id, _) /*check only*/
              .flatMap: _ =>
                coll:
                  orderId <-: OrderLocksQueued(instr.demands)
        else
          coll.nix

  override def onReturnFromSubworkflow[S <: EngineState_[S]](instr: Instr, order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll:
        coll.aggregate.atController(order.id):
          OrderLocksReleased(instr.lockPaths)
