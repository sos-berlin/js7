package com.sos.scheduler.engine.agent.task

import com.google.common.base.Splitter
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.{StartTask, StartApiTask, StartNonApiTask}
import com.sos.scheduler.engine.agent.task.StandardAgentTaskFactory._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.SimpleTaskServer
import com.sos.scheduler.engine.taskserver.task.{OwnProcessTaskServer, TaskStartArguments}
import com.sos.scheduler.engine.tunnel.TunnelHandler
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.util.concurrent.ThreadLocalRandom
import java.util.regex.Pattern
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentTaskFactory @Inject private(agentConfiguration: AgentConfiguration, tunnelHandler: TunnelHandler) extends AgentTaskFactory {

  private val agentTaskIdGenerator = newAgentTaskIdGenerator()

  def apply(command: StartTask) = {
    val id = agentTaskIdGenerator.next()
    val address = tunnelHandler.proxyAddressString
    val tunnel = tunnelHandler.newTunnel(TunnelId(id.index.toString))
    new AgentTask(id, tunnel, newTaskServer(id, command, address, tunnel.tunnelToken))
  }

  private def newTaskServer(id: AgentTaskId, command: StartTask, controllerAddress: String, tunnelToken: TunnelToken) = {
    val taskStartArguments = TaskStartArguments(
      controllerAddress = controllerAddress,
      tunnelToken = tunnelToken,
      directory = agentConfiguration.directory,
      environment = agentConfiguration.environment)
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
  private val MaxIndex = Int.MaxValue

  /**
   * Delivers [[AgentTaskId]] with recognizable increasing numbers.
   * The increasing number is meaningless.
   */
  private[task] def newAgentTaskIdGenerator(start: Int = 1): Iterator[AgentTaskId] = {
    val numbers = Iterator.range(start, MaxIndex) ++ Iterator.continually { Iterator.range(1, MaxIndex) }.flatten
    numbers map { i ⇒ AgentTaskId(index = i, salt = ThreadLocalRandom.current.nextLong()) }
  }
}
