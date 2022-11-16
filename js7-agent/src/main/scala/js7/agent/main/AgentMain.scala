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
import js7.common.system.startup.StartUp.{nowString, printlnWithClock}
import js7.common.system.startup.{Js7ReturnCodes, StartUp}
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.subagent.BareSubagent
import scala.concurrent.duration.{Deadline, Duration, NANOSECONDS}

/**
 * JS7 Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentMain
{
  private lazy val logger = Logger(getClass)

  def run(arguments: CommandLineArguments): ProgramTermination = {
    // Log early for early timestamp and proper logger initialization by a single (non-parallel) call
    logger.info("JS7 Agent " + BuildInfo.longVersion +
      "\n" + "━" * 80)  // In case, the previous file is appended
    logger.info(StartUp.startUpLine())
    logger.debug(arguments.toString)

    val agentConf = AgentConfiguration.fromCommandLine(arguments)
    logger.info(s"config=${agentConf.configDirectory} data=${agentConf.dataDirectory}")
    if (agentConf.scriptInjectionAllowed) logger.info("SIGNED SCRIPT INJECTION IS ALLOWED")
    logConfig(agentConf.config)
    StartUp.logJavaSettings()

    val terminated = blockingRun(agentConf)

    // Log complete timestamp in case of short log timestamp
    val msg = s"JS7 Subagent terminates now ($nowString)"
    logger.info(msg)
    printlnWithClock(msg)
    terminated
  }

  /** Run as an Agent Director or as a bare Subagent. */
  private def blockingRun(agentConf: AgentConfiguration): ProgramTermination =
    agentConf.journalMeta.currentFile match {
      case Left(_) =>
        BareSubagent
          .blockingRun(agentConf.subagentConf)
          .getOrElse {
            logger.info("Continue as Agent Director\n" + "─" * 80)
            blockingRunAgentDirector(agentConf)
          }

      case Right(_) =>
        logger.debug("Start as Agent Director with existing journal")
        blockingRunAgentDirector(agentConf)
    }

  private def blockingRunAgentDirector(agentConf: AgentConfiguration): ProgramTermination = {
    printlnWithClock("Continue as Agent Director")
    var termination = ProgramTermination()
    val agent = RunningAgent(agentConf).awaitInfinite
    autoClosing(agent) { _ =>
      withShutdownHooks(agentConf.config, "AgentMain", () => onJavaShutdown(agent)) {
        termination = agent.terminated.awaitInfinite
      }
    }
    termination
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
  var _runningSince: Option[Deadline] = None

  def runningSince =
    _runningSince

  def main(args: Array[String]): Unit = {
    val nanoTime = System.nanoTime() // Before anything else, fetch clock
    printlnWithClock(s"JS7 Agent ${BuildInfo.longVersion}")
    _runningSince = Some(Deadline(Duration(nanoTime, NANOSECONDS)))
    StartUp.initializeMain()

    var terminated = ProgramTermination()
    lockAndRunMain(args) { commandLineArguments =>
      terminated = new AgentMain().run(commandLineArguments)
    }
    if (terminated.restart) {
      System.exit(Js7ReturnCodes.Restart)
    }
  }
}
