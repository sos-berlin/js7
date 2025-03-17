package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.ChangeGlobalToPlannableBoard
import js7.data.controller.ControllerStatePlanFunctions
import js7.data.event.EventCalc
import js7.data.plan.PlanSchemaId

private[command] def changeGlobalToPlannableBoardExecutor = 
  ToEventCalc[ChangeGlobalToPlannableBoard]: cmd =>
    EventCalc: coll =>
      coll.addChecked:
        import cmd.{planSchemaId, plannableBoard}
        ControllerStatePlanFunctions.changeBoardType(
          plannableBoard,
          fromPlanSchemaId = PlanSchemaId.Global,
          toPlanSchemaId = planSchemaId,
          endOfLife = None,
          coll.aggregate
        ):
          plannedNoticeKey =>
            cmd.evalSplitNoticeKey(plannedNoticeKey.noticeKey).map: (planKey, noticeKey) =>
              Some(planSchemaId / planKey / noticeKey)
