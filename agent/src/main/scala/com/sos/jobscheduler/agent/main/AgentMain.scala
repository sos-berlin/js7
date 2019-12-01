package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTermination
import com.sos.jobscheduler.agent.data.commands.AgentCommand.ShutDown
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs.logConfig
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.core.startup.JavaMain.withShutdownHooks
import com.sos.jobscheduler.core.startup.JavaMainLockfileSupport.lockAndRunMain
import java.time.LocalTime
import scala.concurrent.duration._

/**
 * JobScheduler Agent Server.
 *
 * @author Joacim Zschimmer
 */
final class AgentMain
{
  private val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): AgentTermination.Terminate = {
    logger.info(s"JobScheduler Agent Server ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    logger.debug(arguments.toString)
    val agentConfiguration = AgentConfiguration.fromCommandLine(arguments)
    logConfig(agentConfiguration.config)
    var terminated = AgentTermination.Terminate()
    autoClosing(RunningAgent(agentConfiguration).awaitInfinite) { agent =>
      withShutdownHooks(agentConfiguration.config, "AgentMain", onJavaShutdown(agent)) {
        terminated = agent.terminated.awaitInfinite
      }
    }
    val msg = "JobScheduler Agent Server terminates"
    logger.info(msg)
    println(msg)
    terminated
  }

  private def onJavaShutdown(agent: RunningAgent)(timeout: FiniteDuration): Unit = {
    logger.warn("Trying to shut down JobScheduler Agent Server due to Java shutdown")
    import agent.scheduler
    val sigkillAfter = agent.config.getDuration("jobscheduler.termination.sigkill-after").toFiniteDuration
    agent.executeCommand(ShutDown(sigtermProcesses = true, sigkillProcessesAfter = Some(sigkillAfter))).runAsyncAndForget
    agent.terminated await timeout
    agent.close()
  }
}

object AgentMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile

  def main(args: Array[String]): Unit = {
    println(s"${LocalTime.now.toString take 12} JobScheduler Agent Server ${BuildInfo.prettyVersion}")
    var terminated = AgentTermination.Terminate()
    lockAndRunMain(args) { commandLineArguments =>
      terminated = new AgentMain().run(commandLineArguments)
    }
    if (terminated.restart) {
      System.exit(97)
    }
  }
}
