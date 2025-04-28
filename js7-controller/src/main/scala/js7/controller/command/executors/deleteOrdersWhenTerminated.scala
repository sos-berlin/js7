package js7.controller.command.executors

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.Problems.{CannotDeleteChildOrderProblem, CannotDeleteWatchingOrderProblem}
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] def deleteOrdersWhenTerminatedExecutor
: CommandEventConverter[DeleteOrdersWhenTerminated] =
  CommandEventConverter.checked[DeleteOrdersWhenTerminated]: (cmd, controllerState) =>
    val instrService = InstructionExecutorService(EventCalc.clock)
    val orderEventSource = new OrderEventSource(controllerState)(using instrService)
    cmd.orderIds.toVector
      .traverse(controllerState.idToOrder.checked)
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
