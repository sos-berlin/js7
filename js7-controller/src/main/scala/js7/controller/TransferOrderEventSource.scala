package js7.controller

import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.controller.ControllerCommand.TransferOrders
import js7.data.controller.ControllerState
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.workflow.Workflow

final class TransferOrderEventSource(controllerState: ControllerState):

  def transferOrders(cmd: TransferOrders): Checked[Seq[KeyedEvent[OrderActorEvent]]] =
    controllerState.repo.pathTo(Workflow)(cmd.workflowId.path)
      .flatMap: toWorkflow =>
        if controllerState.repo.isCurrentItem(cmd.workflowId) then
          Left(Problem.pure(s"${cmd.workflowId} is already the current version"))
        else
          controllerState
            .workflowToOrders.workflowIdToOrders.get(cmd.workflowId)
            .toVector
            .flatten
            .sorted // For testable error message and readable logging
            .map(controllerState.idToOrder)
            .map: order =>
              transferOrder(order, toWorkflow)
            .combineProblems
            .map(_.flatten)

  private def transferOrder(order: Order[Order.State], toWorkflow: Workflow)
  : Checked[Seq[KeyedEvent[OrderActorEvent]]] =
    for
      fromWorkflow <- controllerState.idToWorkflow.checked(order.workflowId)
      events <- order.transfer(fromWorkflow, toWorkflow)
    yield
      events.map(order.id <-: _)
