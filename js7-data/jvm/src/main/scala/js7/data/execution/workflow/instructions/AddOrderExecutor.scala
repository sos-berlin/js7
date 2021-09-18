package js7.data.execution.workflow.instructions

import js7.base.problem.Problems.DuplicateKey
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderOrderAdded}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.AddOrder
import scala.collection.View

private[instructions] final class AddOrderExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = AddOrder

  def toEvents(addOrder: AddOrder, order: Order[Order.State], state: StateView) = {
    detach(order)
      .orElse(start(order))
      .getOrElse(
        order.state match {
          case Order.Ready =>
            val events = for {
              workflowId <- state.workflowPathToId(addOrder.workflowPath)
              scope <- state.toScope(order)
              addedOrderId <- addOrder.orderId.evalAsString(scope).flatMap(OrderId.checked)
              args <- addOrder.arguments.eval(scope).flatMap(_.asObjectValue)
            } yield
              if (state.idToOrder.isDefinedAt(addedOrderId))
                View(OrderFailedIntermediate_(
                  Some(Outcome.Failed.fromProblem(
                    DuplicateKey("OrderId", addedOrderId.string)))))
              else
                View(
                  OrderOrderAdded(addedOrderId, workflowId, args.nameToValue,
                    deleteWhenTerminated = addOrder.deleteWhenTerminated),
                  OrderMoved(order.position.increment))
            events.map(_.map(order.id <-: _).toList)
          case _ => Right(Nil)
        })
  }
}