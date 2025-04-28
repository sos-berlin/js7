package js7.controller.command.executors

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.TransferOrders
import js7.data.workflow.Workflow

private[command] def transferOrdersExecutor: CommandEventConverter[TransferOrders] =
  CommandEventConverter.checked[TransferOrders]: (cmd, controllerState) =>
    if controllerState.repo.isCurrentItem(cmd.workflowId) then
      Left(Problem.pure(s"${cmd.workflowId} is already the current version"))
    else
      controllerState.repo.pathTo(Workflow)(cmd.workflowId.path).flatMap: toWorkflow =>
        controllerState.workflowToOrders.workflowIdToOrders.get(cmd.workflowId)
          .toVector
          .flatten
          .sorted // For testable error message and readable logging
          .map(controllerState.idToOrder)
          .map: order =>
            for
              fromWorkflow <- controllerState.idToWorkflow.checked(order.workflowId)
              events <- order.transfer(fromWorkflow, toWorkflow)
            yield
              events.map(order.id <-: _)
          .combineProblems
          .map(_.flatten)
