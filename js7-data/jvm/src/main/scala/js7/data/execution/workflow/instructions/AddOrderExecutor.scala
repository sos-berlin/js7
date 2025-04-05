package js7.data.execution.workflow.instructions

import cats.syntax.semigroup.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.RichOption
import js7.data.controller.ControllerState
import js7.data.event.EventColl
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderMoved, OrderOrderAdded}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.plan.PlanId
import js7.data.state.{StateView, UniqueOrderIdScope}
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.workflow.instructions.AddOrder

private[instructions] final class AddOrderExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = AddOrder
  val instructionClass = classOf[AddOrder]

  def toEvents(addOrder: AddOrder, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .getOrElse:
        order.state match
          case _: Order.Ready =>
            for
              controllerState <- checkedCast[ControllerState](state)
              workflowId <- state.workflowPathToId(addOrder.workflowPath)
              scope <- state.toImpureOrderExecutingScope(order, clock.now())
              planId <- addOrder.planId.fold_(Checked(order.planId), PlanId.evalPlanIdExpr(_, scope))
              addedOrderId <- addOrder.orderId.evalAsString:
                scope |+| UniqueOrderIdScope(controllerState.idToOrder.keySet)
              addedOrderId <- OrderId.checked(addedOrderId)
              args <- evalExpressionMap(addOrder.arguments, scope)
              //computedPlanId <- controllerState.evalOrderToPlanId(Order.fromOrderAdded(addedOrderId, orderAdded))
              orderAdded = OrderOrderAdded(addedOrderId, workflowId,
                args,
                planId = planId,
                innerBlock = addOrder.innerBlock,
                startPosition = addOrder.startPosition,
                stopPositions = addOrder.stopPositions,
                deleteWhenTerminated = addOrder.deleteWhenTerminated,
                forceJobAdmission = addOrder.forceJobAdmission)
              keyedEvents <- controllerState
                .checkPlanAcceptsOrders(
                  planId,
                  allowClosedPlan = planId == order.planId)
                .flatMap: _ =>
                  EventColl(controllerState).add[OrderOrderAdded | OrderMoved]:
                    order.id <-: orderAdded
                .match
                  case Left(problem) =>
                    // OrderFailedIntermediate_ is not applicable, it is intermediate only
                    Right(List(order.id <-:
                      OrderFailedIntermediate_(Some(OrderOutcome.Failed.fromProblem(problem)))))
                  case Right(coll) =>
                    coll.add:
                      order.id <-: OrderMoved(order.position.increment)
                    .map(_.keyedEvents)
                .map(_.toList)
            yield
              keyedEvents

          case _ => Right(Nil)
