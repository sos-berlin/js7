package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.board.PlannedNoticeKey
import js7.data.controller.ControllerCommand.ChangePlannableToGlobalBoard
import js7.data.controller.ControllerStatePlanFunctions
import js7.data.event.EventCalc
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.data.value.expression.scopes.NowScope

private[command] def changePlannableToGlobalBoardExecutor
: CommandEventConverter[ChangePlannableToGlobalBoard] =
  CommandEventConverter.checked[ChangePlannableToGlobalBoard]: (cmd, controllerState) =>
    import cmd.{globalBoard, planSchemaId}
    globalBoard.evalEndOfLife(NowScope(EventCalc.now)).flatMap: endOfLife =>
      ControllerStatePlanFunctions.changeBoardType(
        globalBoard,
        fromPlanSchemaId = planSchemaId,
        toPlanSchemaId = PlanSchemaId.Global,
        endOfLife,
        controllerState
      ):
        case PlannedNoticeKey(PlanId(`planSchemaId`, planKey), noticeKey) =>
          cmd.evalMakeNoticeKey(planKey, noticeKey).map: noticeKey =>
            Some(PlanId.Global / noticeKey)
        case _ => Right(None) // Alien planSchemaId
