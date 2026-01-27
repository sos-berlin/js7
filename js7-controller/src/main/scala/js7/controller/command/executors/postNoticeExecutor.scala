package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.board.NoticeEventSource
import js7.data.controller.ControllerCommand.PostNotice

private[command] def postNoticeExecutor: CommandEventConverter[PostNotice] =

  val noticeEventSource = NoticeEventSource(forCommand = true)

  CommandEventConverter.coll[PostNotice]: (cmd, coll) =>
    coll:
      noticeEventSource.executePostNoticeCommand(cmd)
