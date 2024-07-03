package js7.data.execution.workflow.instructions

import js7.base.problem.Problems.DuplicateKey
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderOrderAdded}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.state.StateView
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
        order.state match
          case Order.Ready =>
            val events = for
              workflowId <- state.workflowPathToId(addOrder.workflowPath)
              scope <- state.toImpureOrderExecutingScope(order, clock.now())
              addedOrderId <- addOrder.orderId.evalAsString(scope).flatMap(OrderId.checked)
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
            events.map(_.map(order.id <-: _).toList)

          case _ => Right(Nil)
