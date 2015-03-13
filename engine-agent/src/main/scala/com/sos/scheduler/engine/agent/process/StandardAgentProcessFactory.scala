package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.commands.{StartDedicatedProcess, StartProcess, StartThread}
import com.sos.scheduler.engine.data.agent.AgentProcessId
import com.sos.scheduler.engine.taskserver.SimpleTaskServer
import com.sos.scheduler.engine.taskserver.task.{DedicatedProcessTaskServer, TaskStartArguments}
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentProcessFactory @Inject private extends AgentProcessFactory {

  private val agentProcessIdGenerator = AgentProcessId.newGenerator()

  def apply(command: StartProcess) = new AgentProcess(agentProcessIdGenerator.next(), newTaskServer(command))

  private def newTaskServer(command: StartProcess) = {
    val taskStartArguments = TaskStartArguments(controllerAddress = command.controllerAddress)
    command match {
      case _: StartThread ⇒ new SimpleTaskServer(taskStartArguments)
      case o: StartDedicatedProcess ⇒ new DedicatedProcessTaskServer(taskStartArguments, javaOptions = o.javaOptions, javaClasspath = o.javaClasspath)
    }
  }
}
