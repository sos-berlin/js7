package js7.controller.command

import com.typesafe.config.Config
import js7.data.command.CommandToEventCalc
import js7.data.controller.ControllerState
import js7.data.event.{Event, TimeCtx}
import scala.collection.immutable.ArraySeq

final class ControllerCommandToEventCalc(config: Config)
extends CommandToEventCalc[ControllerState, Event, TimeCtx]:

  protected val cmdExecutors =
    ArraySeq(
      executors.addOrderExecutor(config),
      executors.addOrdersExecutor(config),
      executors.answerOrderPromptExecutor,
      executors.cancelOrdersExecutor,
      executors.changeGlobalToPlannableBoardExecutor,
      executors.changePlannableToGlobalBoardExecutor,
      executors.changePlanExecutor,
      executors.changePlanSchemaExecutor,
      executors.controlWorkflowExecutor,
      executors.controlWorkflowPathExecutor,
      executors.deleteNoticeExecutor,
      executors.deleteOrdersWhenTerminatedExecutor,
      executors.emitTestEventExecutor,
      executors.goOrderExecutor,
      executors.postNoticeExecutor,
      executors.resumeOrderExecutor,
      executors.resumeOrdersExecutor,
      executors.suspendOrdersExecutor,
      executors.transferOrdersExecutor)


object ControllerCommandToEventCalc
extends CommandToEventCalc.Companion[ControllerState, Event, TimeCtx]
