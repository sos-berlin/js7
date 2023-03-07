package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.log.Logger
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.StartUp.{nowString, printlnWithClock}
import js7.common.system.startup.{Js7ReturnCodes, ServiceMain, StartUp}
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
    // Log early for early timestamp and proper logger initialization by a
    // single (non-concurrent) call
    // Log a bar, in case the previous file is appended
    logger.info("JS7 Agent " + BuildInfo.longVersion +
      "\n" + "━" * 80)
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

  private def blockingRunAgentDirector(conf: AgentConfiguration): ProgramTermination = {
    printlnWithClock("Continue as Agent Director")
    ServiceMain.blockingRun("Agent", conf.config, RunningAgent.resource(conf)(_))
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
