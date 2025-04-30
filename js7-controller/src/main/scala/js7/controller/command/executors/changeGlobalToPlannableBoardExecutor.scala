package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.ChangeGlobalToPlannableBoard
import js7.data.controller.ControllerStatePlanFunctions
import js7.data.plan.PlanSchemaId

private[command] def changeGlobalToPlannableBoardExecutor
: CommandEventConverter[ChangeGlobalToPlannableBoard] =
  CommandEventConverter.checked[ChangeGlobalToPlannableBoard]: (cmd, controllerState) =>
    import cmd.{planSchemaId, plannableBoard}
    ControllerStatePlanFunctions.changeBoardType(
      plannableBoard,
      fromPlanSchemaId = PlanSchemaId.Global,
      toPlanSchemaId = planSchemaId,
      endOfLife = None,
      controllerState
    ):
      plannedNoticeKey =>
        cmd.evalSplitNoticeKey(plannedNoticeKey.noticeKey).map: (planKey, noticeKey) =>
          Some(planSchemaId / planKey / noticeKey)
