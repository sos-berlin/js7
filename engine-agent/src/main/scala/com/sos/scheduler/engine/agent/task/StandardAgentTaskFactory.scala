package com.sos.scheduler.engine.agent.task

import com.google.common.base.Splitter
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.{StartApiTask, StartNonApiTask, StartTask}
import com.sos.scheduler.engine.agent.task.StandardAgentTaskFactory._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.{OwnProcessTaskServer, SimpleTaskServer}
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelServer
import java.util.regex.Pattern
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentTaskFactory @Inject private(agentConfiguration: AgentConfiguration, tunnelServer: TunnelServer)
extends AgentTaskFactory {

  private val agentTaskIdGenerator = AgentTaskId.newGenerator()

  def apply(command: StartTask) = {
    val id = agentTaskIdGenerator.next()
    val address = tunnelServer.proxyAddressString
    val tunnel = tunnelServer.newTunnel(TunnelId(id.index.toString))
    new AgentTask(id, tunnel, newTaskServer(id, command, address, tunnel.tunnelToken))
    // AgentTask closes Tunnel and TaskServer
  }

  private def newTaskServer(agentTaskId: AgentTaskId, command: StartTask, masterAddress: String, tunnelToken: TunnelToken) = {
    val taskStartArguments = TaskStartArguments(
      agentTaskId,
      masterAddress = masterAddress,
      tunnelToken = tunnelToken,
      directory = agentConfiguration.directory,
      environment = agentConfiguration.environment,
      killScriptFileOption = agentConfiguration.killScriptFile)
    val taskServer =
      if (sys.props contains UseThreadPropertyName) { // For debugging
        logger.warn(s"Due to system property $UseThreadPropertyName, task does not use an own process")
        new SimpleTaskServer(taskStartArguments)
      } else
        command match {
          case StartNonApiTask ⇒ new SimpleTaskServer(taskStartArguments)
          case o: StartApiTask ⇒ new OwnProcessTaskServer(
            taskStartArguments,
            javaOptions = agentConfiguration.jobJavaOptions ++ splitJavaOptions(o.javaOptions),
            javaClasspath = o.javaClasspath)
        }
    taskServer
  }

  private def splitJavaOptions(options: String) =
    Splitter.on(Pattern.compile("\\s+")).trimResults.omitEmptyStrings.split(options).toImmutableSeq
}

private object StandardAgentTaskFactory {
  private val logger = Logger(getClass)
  private val UseThreadPropertyName = "jobscheduler.agent.useThread"
}
