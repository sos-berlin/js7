package js7.data.execution.workflow.instructions

import cats.syntax.semigroup.*
import js7.base.problem.Problems.DuplicateKey
import js7.data.controller.ControllerState
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderOrderAdded}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.state.{StateView, UniqueOrderIdScope}
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.workflow.instructions.AddOrder
import scala.collection.View

private[instructions] final class AddOrderExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = AddOrder
  val instructionClass = classOf[AddOrder]

  def toEvents(addOrder: AddOrder, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .getOrElse:
        val controllerState = state.asInstanceOf[ControllerState]
        order.state match
          case Order.Ready =>
            locally:
              for
                workflowId <- state.workflowPathToId(addOrder.workflowPath)
                scope <- state.toImpureOrderExecutingScope(order, clock.now())
                addedOrderId <- addOrder.orderId.evalAsString:
                  scope |+| UniqueOrderIdScope(controllerState.idToOrder.keySet)
                addedOrderId <- OrderId.checked(addedOrderId)
                args <- evalExpressionMap(addOrder.arguments, scope)
              yield
                if state.idToOrder.isDefinedAt(addedOrderId) then
                  View(OrderFailedIntermediate_(
                    Some(OrderOutcome.Failed.fromProblem(
                      DuplicateKey("OrderId", addedOrderId.string)))))
                else
                  View(
                    OrderOrderAdded(addedOrderId, workflowId, args,
                      innerBlock = addOrder.innerBlock,
                      startPosition = addOrder.startPosition,
                      stopPositions = addOrder.stopPositions,
                      deleteWhenTerminated = addOrder.deleteWhenTerminated,
                      forceJobAdmission = addOrder.forceJobAdmission),
                    OrderMoved(order.position.increment))
            .map(_.map(order.id <-: _).toList)

          case _ => Right(Nil)
