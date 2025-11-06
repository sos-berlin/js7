package js7.controller.command.executors

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.Problems.UnknownOrderProblem
import js7.data.controller.ControllerCommand.{CancelOrders, GoOrder, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{Event, EventCalc, TimeCtx}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.order.OrderId
import js7.data.value.StringValue
import js7.data.value.expression.{ExprFunction, Scope}
import scala.collection.immutable
import scala.collection.immutable.ArraySeq

private[command] def cancelOrdersExecutor: CommandEventConverter[CancelOrders] =
  CommandEventConverter.eventCalc[CancelOrders]: cmd =>
    EventCalc: coll =>
      cmd.orderIds.match
        case orderIds: immutable.Iterable[OrderId] =>
          Right(orderIds)

        case fun: ExprFunction =>
          // Maybe SLOW with a complicated ExprFunction and a million orders !!!
          coll.aggregate.idToOrder.keys.toVector.traverse: orderId =>
            fun.eval(StringValue(orderId.string))(using Scope.empty)
              .flatMap(_.asBoolean)
              .map(orderId -> _)
          .map:
            _.collect:
              case (orderId, true) => orderId
      .map: orderIds =>
        executeOrderMarkCommands(orderIds):
          _.cancel(_, cmd.mode)
      .flatMap: eventCalc =>
        coll.addEventCalc(eventCalc)

private[command] def suspendOrdersExecutor: CommandEventConverter[SuspendOrders] =
  CommandEventConverter.eventCalc[SuspendOrders]: cmd =>
    executeOrderMarkCommands(cmd.orderIds):
      _.suspend(_, cmd.mode)

private[command] def goOrderExecutor: CommandEventConverter[GoOrder] =
  CommandEventConverter.eventCalc[GoOrder]: cmd =>
    executeOrderMarkCommands(Vector(cmd.orderId)):
      _.go(_, cmd.position)

private[command] def resumeOrderExecutor: CommandEventConverter[ResumeOrder] =
  CommandEventConverter.eventCalc[ResumeOrder]: cmd =>
    executeOrderMarkCommands(Vector(cmd.orderId)):
      _.resume(_, cmd.position, cmd.historyOperations, cmd.asSucceeded, cmd.restartKilledJob)

private[command] def resumeOrdersExecutor: CommandEventConverter[ResumeOrders] =
  CommandEventConverter.eventCalc[ResumeOrders]: cmd =>
    executeOrderMarkCommands(cmd.orderIds):
      _.resume(_, None, Nil, cmd.asSucceeded, cmd.restartKilledJob)

private def executeOrderMarkCommands[Cmd <: ControllerCommand](
  orderIds: Iterable[OrderId])
  (toEvents: (OrderEventSource, OrderId) => Checked[List[OrderActorEvent]])
: EventCalc[ControllerState, Event, TimeCtx] =
  EventCalc.checked: controllerState =>
    if !orderIds.areUnique then
      Left(Problem.pure("OrderIds must be unique"))
    else
      // Event may be inserted between events coming from Agent
      val instrService = InstructionExecutorService(EventCalc.clock)
      val orderEventSource = OrderEventSource(controllerState)(using instrService)
      orderIds.toVector.traverse: orderId =>
        controllerState.idToOrder.rightOr(orderId, UnknownOrderProblem(orderId))
      .map: orders =>
        orders.flatTraverse: order =>
          toEvents(orderEventSource, order.id).map: events =>
            events.toVector.map(order.id <-: _)
      .flatten
