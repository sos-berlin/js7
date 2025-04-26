package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.ControlWorkflow
import js7.data.controller.ControllerStateExecutor
import js7.data.event.EventCalc
import js7.data.event.KeyedEvent.NoKey
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId}

private[command] def controlWorkflowExecutor =
  CommandEventConverter.checked[ControlWorkflow]: (cmd, controllerState) =>
    val workflowControlId = WorkflowControlId(cmd.workflowId)
    controllerState.repo.idTo(Workflow)(cmd.workflowId).map: _ =>
      val (item0, isNew) = controllerState
        .keyTo(WorkflowControl)
        .get(workflowControlId) match
        case None => WorkflowControl(workflowControlId) -> true
        case Some(o) => o -> false
      val item = item0.nextRevision.copy(
        breakpoints = item0.breakpoints -- cmd.removeBreakpoints ++ cmd.addBreakpoints)

      val event = if isNew then UnsignedItemAdded(item) else UnsignedItemChanged(item)
      Vector(event)
        .concat:
          val instrService = InstructionExecutorService(EventCalc.clock)
          ControllerStateExecutor(controllerState)(using instrService)
            .updatedWorkflowControlAttachedEvents(item)
        .map(NoKey <-: _)
