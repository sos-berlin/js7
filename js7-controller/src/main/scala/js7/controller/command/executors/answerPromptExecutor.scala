package js7.controller.command.executors

import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.AnswerOrderPrompt
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] def answerOrderPromptExecutor = ToEventCalc[AnswerOrderPrompt]: cmd =>
  EventCalc: coll =>
    coll.addChecked:
      val instrService = InstructionExecutorService(coll.context.clock)
      OrderEventSource(coll.aggregate)(using instrService)
        .answerPrompt(cmd.orderId)
