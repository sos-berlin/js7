package com.sos.scheduler.engine.agent.process

import com.google.common.base.Splitter
import com.sos.scheduler.engine.agent.AgentConfiguration
import com.sos.scheduler.engine.agent.commands.{StartSeparateProcess, StartProcess, StartThread}
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.data.agent.AgentProcessId
import com.sos.scheduler.engine.taskserver.SimpleTaskServer
import com.sos.scheduler.engine.taskserver.task.{SeparateProcessTaskServer, TaskStartArguments}
import java.util.regex.Pattern
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentProcessFactory @Inject private(agentConfiguration: AgentConfiguration) extends AgentProcessFactory {

  private val agentProcessIdGenerator = AgentProcessId.newGenerator()

  def apply(command: StartProcess) = new AgentProcess(agentProcessIdGenerator.next(), newTaskServer(command))

  private def newTaskServer(command: StartProcess) = {
    val taskStartArguments = TaskStartArguments(
      controllerAddress = command.controllerAddress,
      environment = agentConfiguration.environment)
    command match {
      case _: StartThread ⇒ new SimpleTaskServer(taskStartArguments)
      case o: StartSeparateProcess ⇒ new SeparateProcessTaskServer(
        taskStartArguments,
        javaOptions = splitJavaOptions(o.javaOptions),
        javaClasspath = o.javaClasspath)
    }
  }

  private def splitJavaOptions(options: String) =
    Splitter.on(Pattern.compile("\\s+")).trimResults.omitEmptyStrings.split(options).toImmutableSeq
}
