package js7.controller.command.executors

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.Problems.{CannotDeleteChildOrderProblem, CannotDeleteWatchingOrderProblem}
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] def deleteOrdersWhenTerminatedExecutor = ToEventCalc[DeleteOrdersWhenTerminated]: cmd =>
  EventCalc: coll =>
    val instrService = InstructionExecutorService(coll.context.clock)
    val orderEventSource = new OrderEventSource(coll.aggregate)(using instrService)
    coll.addChecked:
      cmd.orderIds.toVector
        .traverse(coll.aggregate.idToOrder.checked)
        .traverse: orders =>
          orders.traverse: order =>
            if order.parent.isDefined then
              Left(CannotDeleteChildOrderProblem(order.id): Problem)
            else if order.hasNonVanishedExternalOrder then
              Left(CannotDeleteWatchingOrderProblem(order.id): Problem)
            else
              Right(order)
        .flatten
        .traverse(_
          .filterNot(_.deleteWhenTerminated)
          .traverse:
            orderEventSource.orderDeletionEvent)
        .flatten
        .map(_.flatten)
