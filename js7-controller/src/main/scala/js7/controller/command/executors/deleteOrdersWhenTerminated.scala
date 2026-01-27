package js7.controller.command.executors

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits.RichIterableOnce
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.Problems.{CannotDeleteChildOrderProblem, CannotDeleteWatchingOrderProblem}
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.execution.workflow.OrderEventSource

private[command] def deleteOrdersWhenTerminatedExecutor
: CommandEventConverter[DeleteOrdersWhenTerminated] =
  CommandEventConverter.coll[DeleteOrdersWhenTerminated]: (cmd, coll) =>
    cmd.orderIds.toVector
      .traverse:
        coll.aggregate.idToOrder.checked
      .traverse: orders =>
        orders.traverse: order =>
          if order.parent.isDefined then
            Left(CannotDeleteChildOrderProblem(order.id): Problem)
          else if order.hasNonVanishedExternalOrder then
            Left(CannotDeleteWatchingOrderProblem(order.id): Problem)
          else
            Right(order)
      .flatten
      .flatMap: orders =>
        coll:
          orders.filterNot(_.deleteWhenTerminated).map:
            OrderEventSource.orderDeletionEvent
          .foldMonoids
