package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.log.Logger
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.ServiceMain
import js7.common.system.startup.StartUp.printlnWithClock
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.subagent.BareSubagent

/**
 * JS7 Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentMain
{
  private lazy val logger = Logger(getClass)

  private def run(commandLineArguments: CommandLineArguments, conf: AgentConfiguration): Int = {
    ServiceMain.withLogger.logFirstLines("Agent", commandLineArguments, conf)
    if (conf.scriptInjectionAllowed) logger.info("SIGNED SCRIPT INJECTION IS ALLOWED")

    ServiceMain.handleProgramTermination("Agent") {
      blockingRun(conf)
    }
  }

  /** Run as an Agent Director or as a bare Subagent. */
  private def blockingRun(agentConf: AgentConfiguration): ProgramTermination =
    if (agentConf.journalMeta.currentFile.isRight) {
      logger.debug("Start as Agent Director with existing journal")
      blockingRunDirector(agentConf)
    } else
      BareSubagent
        .blockingRun(agentConf.subagentConf)
        .getOrElse(
          blockingRunDirector(agentConf))

  private def blockingRunDirector(conf: AgentConfiguration): ProgramTermination = {
    logger.info("Continue as Agent Director\n" + "─" * 80)
    printlnWithClock("Continue as Agent Director")
    ServiceMain.withLogger.blockingRun("Agent", conf.config)(
      service = RunningAgent.resource(conf)(_))
  }
}

object AgentMain
{
  // No Logger here!

  def main(args: Array[String]): Unit = {
    ServiceMain.startUp("Agent")

    val exitCode = lockAndRunMain(args) { commandLineArguments =>
      val conf = AgentConfiguration.fromCommandLine(commandLineArguments)
      new AgentMain().run(commandLineArguments, conf)
    }
    if (exitCode != 0) {
      System.exit(exitCode)
    }
  }
}
