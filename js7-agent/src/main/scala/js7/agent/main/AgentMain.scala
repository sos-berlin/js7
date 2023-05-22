package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.common.system.startup.ServiceMain

object AgentMain
{
  // No Logger here!

  def main(args: Array[String]): Unit =
    ServiceMain.mainThenExit(
      args, "Agent", AgentConfiguration.fromCommandLine(_), useLockFile = true
    )((conf, scheduler) => RunningAgent.resource(conf)(scheduler))
}
