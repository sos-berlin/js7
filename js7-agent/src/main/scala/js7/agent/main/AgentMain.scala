package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentTermination
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.BuildInfo
import js7.base.process.ProcessSignal.SIGTERM
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.commandline.CommandLineArguments
import js7.common.configutils.Configs.logConfig
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.Logger
import js7.common.system.startup.JavaMain.withShutdownHooks
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.StartUp.printlnWithClock

/**
 * JS7 Agent Server.
 *
 * @author Joacim Zschimmer
 */
final class AgentMain
{
  private val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): AgentTermination.Terminate = {
    logger.info(s"JS7 JobScheduler Agent Server ${BuildInfo.prettyVersion}")  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    logger.debug(arguments.toString)
    val agentConfiguration = AgentConfiguration.fromCommandLine(arguments)
    logConfig(agentConfiguration.config)
    var terminated = AgentTermination.Terminate()
    autoClosing(RunningAgent(agentConfiguration).awaitInfinite) { agent =>
      withShutdownHooks(agentConfiguration.config, "AgentMain", () => onJavaShutdown(agent)) {
        terminated = agent.terminated.awaitInfinite
      }
    }
    // Log complete timestamp in case of short log timestamp
    val msg = s"JS7 JobScheduler Agent Server terminates at ${Timestamp.now.show}"
    logger.info(msg)
    printlnWithClock(msg)
    terminated
  }

  private def onJavaShutdown(agent: RunningAgent): Unit = {
    logger.warn("Trying to shut down JS7 Agent Server due to Java shutdown")
    import agent.scheduler
    agent.executeCommandAsSystemUser(ShutDown(Some(SIGTERM)))
      .runAsyncUncancelable {
        case Left(throwable) => logger.error(s"onJavaShutdown: ${throwable.toStringWithCauses}",
          throwable.nullIfNoStackTrace)
        case Right(_) =>
      }
    agent.terminated.awaitInfinite
    agent.close()
  }
}

object AgentMain
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile

  def main(args: Array[String]): Unit = {
    printlnWithClock(s"JS7 JobScheduler Agent Server ${BuildInfo.prettyVersion}")
    var terminated = AgentTermination.Terminate()
    lockAndRunMain(args) { commandLineArguments =>
      terminated = new AgentMain().run(commandLineArguments)
    }
    if (terminated.restart) {
      System.exit(97)
    }
  }
}
