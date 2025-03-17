package js7.controller.command.executors

import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.ToEventSeq
import js7.data.controller.ControllerCommand.DeleteNotice

private[command] def deleteNoticeExecutor = ToEventSeq[DeleteNotice]: (cmd, controllerState) =>
  import cmd.noticeId
  for
    plan <- controllerState.plan(noticeId.planId)
    plannedBoard <- plan.toPlannedBoard.checked(noticeId.boardPath)
    _ <- plannedBoard.notice(noticeId.noticeKey)
    keyedEvent <- plannedBoard.deleteNoticeEvent(noticeId.noticeKey)
  yield
    keyedEvent :: Nil
