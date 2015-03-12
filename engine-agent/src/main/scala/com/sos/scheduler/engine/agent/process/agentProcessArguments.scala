package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.data.agent.AgentProcessId
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * @author Joacim Zschimmer
 */
sealed trait AgentProcessArguments {
  def processId: AgentProcessId
  def taskStartArguments: TaskStartArguments
}

final case class DummyProcessArguments(
  processId: AgentProcessId,
  taskStartArguments: TaskStartArguments)
extends AgentProcessArguments

final case class DedicatedProcessArguments(
  processId: AgentProcessId,
  taskStartArguments: TaskStartArguments,
  javaOptions: String,
  javaClasspath: String)
extends AgentProcessArguments
