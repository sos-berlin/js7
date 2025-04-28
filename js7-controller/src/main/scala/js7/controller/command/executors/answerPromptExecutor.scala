package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.AnswerOrderPrompt
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] def answerOrderPromptExecutor: CommandEventConverter[AnswerOrderPrompt] =
  CommandEventConverter.checked[AnswerOrderPrompt]: (cmd, controllerState) =>
    val instrService = InstructionExecutorService(EventCalc.clock)
    OrderEventSource(controllerState)(using instrService)
      .answerPrompt(cmd.orderId)
