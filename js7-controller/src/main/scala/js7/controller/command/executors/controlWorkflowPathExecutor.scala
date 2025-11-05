package js7.controller.command.executors

import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.ControlWorkflowPath
import js7.data.controller.ControllerStateExecutor
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.workflow.{Workflow, WorkflowPathControl, WorkflowPathControlPath}

private[command] def controlWorkflowPathExecutor: CommandEventConverter[ControlWorkflowPath] =
  CommandEventConverter.coll[ControlWorkflowPath]: (cmd, coll) =>
    val path = WorkflowPathControlPath(cmd.workflowPath)
    coll.aggregate.repo.pathToItems(Workflow).checked(cmd.workflowPath).flatMap: _ =>
      val (itemState, isNew) = coll.aggregate
        .pathToUnsignedSimple(WorkflowPathControl)
        .get(path) match
        case None => WorkflowPathControl(path) -> true
        case Some(o) => o -> false
      var item = itemState.item
      item = item.incrementRevision.copy(
        suspended = cmd.suspend.fold(item.suspended)(identity),
        skip = item.skip
          -- cmd.skip.filterNot(_._2).keys
          ++ cmd.skip.filter(_._2).keys)

      for
        coll <- coll.add:
          if isNew then
            NoKey <-: UnsignedSimpleItemAdded(item)
          else
            NoKey <-: UnsignedSimpleItemChanged(item)
        coll <- coll.add:
          ControllerStateExecutor.updatedWorkflowPathControlAttachedEvents(item)
      yield
        coll
