package js7.controller.command.executors

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.TransferOrders
import js7.data.event.EventCalc
import js7.data.workflow.Workflow

private[command] def transferOrdersExecutor = ToEventCalc[TransferOrders]: cmd =>
  EventCalc: coll =>
    coll.addChecked:
      if coll.aggregate.repo.isCurrentItem(cmd.workflowId) then
        Left(Problem.pure(s"${cmd.workflowId} is already the current version"))
      else
        coll.aggregate.repo.pathTo(Workflow)(cmd.workflowId.path).flatMap: toWorkflow =>
          coll.aggregate.workflowToOrders.workflowIdToOrders.get(cmd.workflowId)
            .toVector
            .flatten
            .sorted // For testable error message and readable logging
            .map(coll.aggregate.idToOrder)
            .map: order =>
              for
                fromWorkflow <- coll.aggregate.idToWorkflow.checked(order.workflowId)
                events <- order.transfer(fromWorkflow, toWorkflow)
              yield
                events.map(order.id <-: _)
            .combineProblems
            .map(_.flatten)
