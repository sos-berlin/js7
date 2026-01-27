package js7.controller.command.executors

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.{CancelOrders, GoOrder, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.data.controller.{ControllerCommand, ControllerEventCalc, ControllerState}
import js7.data.event.{Event, EventCalc, TimeCtx}
import js7.data.execution.workflow.OrderEventSource
import js7.data.order.OrderId
import js7.data.value.StringValue
import js7.data.value.expression.{Expression, Scope}
import scala.collection.immutable

private[command] def cancelOrdersExecutor: CommandEventConverter[CancelOrders] =
  CommandEventConverter.eventCalc[CancelOrders]: cmd =>
    EventCalc: coll =>
      cmd.orderIds.match
        case orderIds: immutable.Iterable[OrderId] =>
          Right(orderIds)

        case expr: Expression.FunctionExpr =>
          // Maybe SLOW with a complicated ExprFunction and a million orders !!!
          coll.aggregate.idToOrder.keys.toVector.traverse: orderId =>
            expr.function.eval(StringValue(orderId.string))(using Scope.empty)
              .flatMap(_.asBoolean)
              .map(orderId -> _)
          .map:
            _.collect:
              case (orderId, true) => orderId

        case expr: Expression =>
          // Maybe SLOW with a complicated ExprFunction and a million orders !!!
          coll.aggregate.idToOrder.values.toVector.traverse: order =>
            coll.aggregate.toOrderScope(order).flatMap: scope =>
              expr.eval(using Scope.empty)
                .flatMap(_.asBoolean)
                .map(order.id -> _)
          .map:
            _.collect:
              case (orderId, true) => orderId
      .map: orderIds =>
        executeOrderMarkCommands(orderIds):
          OrderEventSource.cancel(_, cmd.mode).widen
      .flatMap: eventCalc =>
        coll.addEventCalc(eventCalc)

private[command] def suspendOrdersExecutor: CommandEventConverter[SuspendOrders] =
  CommandEventConverter.eventCalc[SuspendOrders]: cmd =>
    executeOrderMarkCommands(cmd.orderIds):
      OrderEventSource.suspend(_, cmd.mode).widen

private[command] def goOrderExecutor: CommandEventConverter[GoOrder] =
  CommandEventConverter.eventCalc[GoOrder]: cmd =>
    executeOrderMarkCommands(Vector(cmd.orderId)):
      OrderEventSource.go(_, cmd.position).widen

private[command] def resumeOrderExecutor: CommandEventConverter[ResumeOrder] =
  CommandEventConverter.eventCalc[ResumeOrder]: cmd =>
    executeOrderMarkCommands(Vector(cmd.orderId)):
      OrderEventSource.resume(_, cmd.position, cmd.historyOperations, cmd.asSucceeded, cmd.restartKilledJob)
        .widen

private[command] def resumeOrdersExecutor: CommandEventConverter[ResumeOrders] =
  CommandEventConverter.eventCalc[ResumeOrders]: cmd =>
    executeOrderMarkCommands(cmd.orderIds):
      OrderEventSource.resume(_, None, Nil, cmd.asSucceeded, cmd.restartKilledJob)
        .widen

private def executeOrderMarkCommands[Cmd <: ControllerCommand](
  orderIds: Iterable[OrderId])
  (toEventCalc: OrderId => ControllerEventCalc)
: EventCalc[ControllerState, Event] =
  if !orderIds.areUnique then
    EventCalc.problem(Problem.pure("OrderIds must be unique"))
  else
    // Event may be inserted between events coming from Agent
    orderIds.view.map(toEventCalc).foldMonoids