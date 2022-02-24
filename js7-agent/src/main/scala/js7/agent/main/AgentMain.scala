package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMain.withShutdownHooks
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.StartUp
import js7.common.system.startup.StartUp.printlnWithClock
import js7.subagent.StandaloneSubagent
import scala.concurrent.duration.{Deadline, Duration, NANOSECONDS}

/**
 * JS7 Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentMain
{
  private val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): ProgramTermination = {
    // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
    logger.info("JS7 Agent " + BuildInfo.longVersion +
      "\n" + "━" * 80)  // In case, the previous file is appended
    logger.info(StartUp.startUpLine())
    logger.debug(arguments.toString)
    val agentConfiguration = AgentConfiguration.fromCommandLine(arguments)
    logger.info(s"config=${agentConfiguration.configDirectory} data=${agentConfiguration.dataDirectory}")
    logConfig(agentConfiguration.config)
    StartUp.logJavaSettings()

    var terminated = ProgramTermination()
    if (agentConfiguration.isStandaloneSubagent)
      StandaloneSubagent.blockingRun(agentConfiguration.subagentConf.finishAndProvideFiles)
    else
      autoClosing(RunningAgent(agentConfiguration).awaitInfinite) { agent =>
        withShutdownHooks(agentConfiguration.config, "AgentMain", () => onJavaShutdown(agent)) {
          terminated = agent.terminated.awaitInfinite
        }
      }

    // Log complete timestamp in case of short log timestamp
    val msg = s"JS7 Agent terminates now"
    logger.info(msg)
    printlnWithClock(msg)
    terminated
  }

  private def onJavaShutdown(agent: RunningAgent): Unit = {
    logger.warn("Trying to shut down JS7 Agent due to Java shutdown")
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
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  var _runningSince: Option[Deadline] = None

  def runningSince =
    _runningSince

  def main(args: Array[String]): Unit = {
    val nanoTime = System.nanoTime() // Before anything else, fetch clock
    printlnWithClock(s"JS7 Agent ${BuildInfo.longVersion}")
    _runningSince = Some(Deadline(Duration(nanoTime, NANOSECONDS)))
    StartUp.initialize()

    var terminated = ProgramTermination()
    lockAndRunMain(args) { commandLineArguments =>
      terminated = new AgentMain().run(commandLineArguments)
    }
    if (terminated.restart) {
      System.exit(97)
    }
  }
}
