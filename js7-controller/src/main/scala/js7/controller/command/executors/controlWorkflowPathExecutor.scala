package js7.controller.command.executors

import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.ControlWorkflowPath
import js7.data.controller.ControllerStateExecutor
import js7.data.event.EventCalc
import js7.data.event.KeyedEvent.NoKey
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.workflow.{Workflow, WorkflowPathControl, WorkflowPathControlPath}

private[command] def controlWorkflowPathExecutor = ToEventCalc[ControlWorkflowPath]: cmd =>
  val path = WorkflowPathControlPath(cmd.workflowPath)
  EventCalc: coll =>
    coll.addChecked:
      coll.aggregate.repo.pathToItems(Workflow).checked(cmd.workflowPath).map: _ =>
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

        val event = if isNew then UnsignedSimpleItemAdded(item) else UnsignedSimpleItemChanged(item)
        Vector(event)
          .concat:
            val instrService = InstructionExecutorService(coll.context.clock)
            ControllerStateExecutor(coll.aggregate)(using instrService)
              .updatedWorkflowPathControlAttachedEvents(item)
          .map(NoKey <-: _)
