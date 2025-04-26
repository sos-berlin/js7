package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.board.NoticeEventSource
import js7.data.controller.ControllerCommand.PostNotice
import js7.data.event.EventCalc

private[command] def postNoticeExecutor =
  CommandEventConverter.checked[PostNotice]: (cmd, controllerState) =>
    NoticeEventSource(EventCalc.clock).executePostNoticeCommand(cmd, controllerState)
