package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.io.process.ReturnCode
import js7.base.log.Logger
import js7.base.time.Timestamp
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMainLockfileSupport.lockAndRunMain
import js7.common.system.startup.StartUp.printlnWithClock
import js7.common.system.startup.{JavaMain, ServiceMain}
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

  private def run(args: CommandLineArguments, conf: AgentConfiguration): ReturnCode = {
    ServiceMain.logging.logFirstLines("Agent", args, conf)
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
    ServiceMain.logging.blockingRun("Agent", conf.config)(RunningAgent.resource(conf)(_))
  }
}

object AgentMain
{
  // No Logger here!
  val startedAt = Timestamp.now

  def main(args: Array[String]): Unit = {
    ServiceMain.startUp("Agent")

    val returnCode = lockAndRunMain(args) { commandLineArguments =>
      val conf = AgentConfiguration.fromCommandLine(commandLineArguments)
      new AgentMain().run(commandLineArguments, conf)
    }
    JavaMain.exitIfNonZero(returnCode)
  }
}
