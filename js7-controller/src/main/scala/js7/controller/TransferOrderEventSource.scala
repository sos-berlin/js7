package js7.controller

import cats.syntax.flatMap.*
import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.data.controller.ControllerCommand.TransferOrders
import js7.data.controller.ControllerState
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderTransferred
import js7.data.workflow.Workflow

final class TransferOrderEventSource(controllerState: ControllerState) {

  def transferOrders(cmd: TransferOrders): Checked[Seq[KeyedEvent[OrderTransferred]]] =
    controllerState.repo.pathTo(Workflow)(cmd.workflowId.path)
      .flatMap(toWorkflow =>
        if (controllerState.repo.isCurrentItem(cmd.workflowId))
          Left(Problem.pure(s"${cmd.workflowId} is already the current version"))
        else
          controllerState
            .workflowToOrders.workflowIdToOrders.get(cmd.workflowId)
            .toVector
            .flatten
            .sorted // For testable error message and readable logging
            .map(controllerState.idToOrder)
            .map(order =>
              checkTransferable(order) >>
                transferOrder(order, toWorkflow)
                  .map(order.id <-: _))
            .combineProblems)

  private def checkTransferable(order: Order[Order.State]): Checked[Unit] =
    order.attachedState match {
      case Some(x) => Left(Problem.pure(s"${order.id} to be transferred is $x"))
      case None => Checked.unit
    }

  private def transferOrder(order: Order[Order.State], toWorkflow: Workflow)
  : Checked[OrderTransferred] =
    for (_ <- toWorkflow.checkPosition(order.position)/*same position exists?*/ ) yield
      OrderTransferred(toWorkflow.id /: order.position)
}
