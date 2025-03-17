package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.EmitTestEvent
import js7.data.controller.ControllerEvent.ControllerTestEvent
import js7.data.event.EventCalc

private[command] def emitTestEventExecutor = ToEventCalc[EmitTestEvent]: cmd =>
  EventCalc.add(ControllerTestEvent)
