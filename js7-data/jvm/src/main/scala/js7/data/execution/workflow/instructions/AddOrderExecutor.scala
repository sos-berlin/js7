package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.data.controller.ControllerState
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderOrderAdded}
import js7.data.order.{Order, OrderId}
import js7.data.plan.PlanId
import js7.data.state.{EngineState_, UniqueOrderIdScope}
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.workflow.instructions.AddOrder

private object AddOrderExecutor extends EventInstructionExecutor_[AddOrder]:

  def toEventCalc[S <: EngineState_[S]](instr: AddOrder, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        order.ifState[Order.Ready].map: order =>
          for
            coll <- coll.narrowAggregate[ControllerState]
            workflowId <- coll.aggregate.workflowPathToId(instr.workflowPath)
            scope <- coll.aggregate.toImpureOrderExecutingScope(order, coll.now)
            addedOrderIdString <- instr.orderId.evalAsString(
              using scope |+| UniqueOrderIdScope(coll.aggregate.idToOrder.keySet))
            planId <- instr.planId.fold(Checked(order.planId))(PlanId.evalPlanIdExpr(_, scope))
            coll <- catchProblemAsOrderFailure(coll, orderId):
              for
                addedOrderId <- OrderId.checked(addedOrderIdString)
                _ <- coll.aggregate
                  .checkPlanAcceptsOrders(planId, allowClosedPlan = planId == order.planId)
                args <- evalExpressionMap(instr.arguments, scope)
                coll <- coll:
                  order.id <-: OrderOrderAdded(
                    addedOrderId, workflowId, args,
                    planId = planId,
                    innerBlock = instr.innerBlock,
                    startPosition = instr.startPosition,
                    stopPositions = instr.stopPositions,
                    deleteWhenTerminated = instr.deleteWhenTerminated,
                    forceAdmission = instr.forceAdmission)
                coll <- coll:
                  moveOrderToNextInstruction(order)
              yield coll
          yield
            coll
        .getOrElse:
          coll.nix
