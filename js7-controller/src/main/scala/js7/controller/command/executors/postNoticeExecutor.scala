package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.board.NoticeEventSource
import js7.data.controller.ControllerCommand.PostNotice
import js7.data.event.EventCalc

private[command] def postNoticeExecutor = ToEventCalc[PostNotice]: cmd =>
  EventCalc: coll =>
    coll.addChecked:
      NoticeEventSource(coll.context.clock)
        .executePostNoticeCommand(cmd, coll.aggregate)
