package js7.agent.command

import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.data.command.CommandToEventCalc
import js7.data.event.Event
import scala.collection.immutable.ArraySeq

final class AgentCommandToEventCalc(conf: AgentConfiguration)
extends CommandToEventCalc[AgentState, Event]:

  protected val cmdExecutors =
    ArraySeq(
      executors.attachOrderExecutor,
      executors.detachOrderExecutor,
      executors.markOrderExecutor,
      executors.releaseEventsExecutor)


object AgentCommandToEventCalc
extends CommandToEventCalc.Companion[AgentState, Event]
