package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.AnswerOrderPrompt
import js7.data.execution.workflow.OrderEventSource

private[command] def answerOrderPromptExecutor: CommandEventConverter[AnswerOrderPrompt] =
  CommandEventConverter.eventCalc[AnswerOrderPrompt]: cmd =>
    OrderEventSource.answerPrompt(cmd.orderId).widen
