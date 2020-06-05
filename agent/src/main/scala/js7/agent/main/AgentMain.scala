package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentTermination
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.BuildInfo
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.common.commandline.CommandLineArguments
import js7.common.configutils.Configs.logConfig
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import js7.core.startup.JavaMain.withShutdownHooks
import js7.core.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.core.startup.StartUp.printlnWithClock
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
    // Log complete timestamp in case of short log timestamp
    val msg = s"JobScheduler Agent Server terminates at ${Timestamp.now.show}"
    logger.info(msg)
    printlnWithClock(msg)
    terminated
  }

  private def onJavaShutdown(agent: RunningAgent)(timeout: FiniteDuration): Unit = {
    logger.warn("Trying to shut down JobScheduler Agent Server due to Java shutdown")
    import agent.scheduler
    val sigkillAfter = agent.config.getDuration("jobscheduler.termination.sigkill-after").toFiniteDuration
    agent.executeCommandAsSystemUser(ShutDown(sigtermProcesses = true, sigkillProcessesAfter = Some(sigkillAfter)))
      .runAsyncAndForget
    agent.terminated await timeout
    agent.close()
  }
}

object AgentMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile

  def main(args: Array[String]): Unit = {
    printlnWithClock(s"JobScheduler Agent Server ${BuildInfo.prettyVersion}")
    var terminated = AgentTermination.Terminate()
    lockAndRunMain(args) { commandLineArguments =>
      terminated = new AgentMain().run(commandLineArguments)
    }
    if (terminated.restart) {
      System.exit(97)
    }
  }
}
