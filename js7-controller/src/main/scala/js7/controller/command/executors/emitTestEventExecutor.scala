package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CmdToEventCalc
import js7.data.controller.ControllerCommand.EmitTestEvent
import js7.data.controller.ControllerEvent.ControllerTestEvent
import js7.data.event.EventCalc

private[command] def emitTestEventExecutor =
  CmdToEventCalc[EmitTestEvent]: cmd =>
    EventCalc.pure(ControllerTestEvent)
