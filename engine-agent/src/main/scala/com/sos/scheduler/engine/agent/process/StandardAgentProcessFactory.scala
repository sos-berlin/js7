package com.sos.scheduler.engine.agent.process

import com.google.common.base.Splitter
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands.{StartProcess, StartSeparateProcess, StartThread}
import com.sos.scheduler.engine.agent.process.StandardAgentProcessFactory._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.SimpleTaskServer
import com.sos.scheduler.engine.taskserver.task.{SeparateProcessTaskServer, TaskStartArguments}
import com.sos.scheduler.engine.tunnel.TunnelHandler
import com.sos.scheduler.engine.tunnel.data.TunnelId
import java.util.concurrent.ThreadLocalRandom
import java.util.regex.Pattern
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentProcessFactory @Inject private(agentConfiguration: AgentConfiguration, tunnelHandler: TunnelHandler) extends AgentProcessFactory {

  private val agentProcessIdGenerator = newAgentProcessIdGenerator()

  def apply(command: StartProcess) = {
    val id = agentProcessIdGenerator.next()
    new AgentProcess(id, newTaskServer(id, command))
  }

  private def newTaskServer(id: AgentProcessId, command: StartProcess) = {

    val (controllerAddress, tunnelOption) = command.controllerAddressOption match {
      case Some(o) ⇒ (o, None)
      case None ⇒
        val address = tunnelHandler.localAddress.getAddress.getHostAddress +":"+ tunnelHandler.localAddress.getPort
        val tunnel = tunnelHandler.newTunnel(TunnelId(id.string))
        (address, Some(tunnel))
    }

    val taskStartArguments = TaskStartArguments(
      controllerAddress = controllerAddress,
      tunnelIdAndPasswordOption = tunnelOption map { _.tunnelToken },
      directory = agentConfiguration.directory,
      environment = agentConfiguration.environment)
    val taskServer =
      if (sys.props contains UseThreadPropertyName) { // For debugging
        logger.warn(s"Due to system property $UseThreadPropertyName, task does not use an own process")
        new SimpleTaskServer(tunnelOption, taskStartArguments)
      } else
        command match {
          case _: StartThread ⇒ new SimpleTaskServer(tunnelOption, taskStartArguments)
          case o: StartSeparateProcess ⇒ new SeparateProcessTaskServer(
            tunnelOption,
            taskStartArguments,
            javaOptions = agentConfiguration.jobJavaOptions ++ splitJavaOptions(o.javaOptions),
            javaClasspath = o.javaClasspath)
        }
    taskServer
  }

  private def splitJavaOptions(options: String) =
    Splitter.on(Pattern.compile("\\s+")).trimResults.omitEmptyStrings.split(options).toImmutableSeq
}

private object StandardAgentProcessFactory {
  private val logger = Logger(getClass)
  private val UseThreadPropertyName = "jobscheduler.agent.useThread"
  private val MaxIndex = Int.MaxValue

  /**
   * Delivers [[AgentProcessId]] with recognizable increasing numbers.
   * The increasing number is meaningless.
   */
  private[process] def newAgentProcessIdGenerator(start: Int = 1): Iterator[AgentProcessId] = {
    val numbers = Iterator.range(start, MaxIndex) ++ Iterator.continually { Iterator.range(1, MaxIndex) }.flatten
    numbers map { i ⇒ AgentProcessId(index = i, salt = ThreadLocalRandom.current.nextLong()) }
  }
}
