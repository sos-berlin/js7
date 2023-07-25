package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.thread.IOExecutor
import js7.common.system.startup.ServiceMain
import monix.eval.Task

object AgentMain
{
  // No Logger here!

  def main(args: Array[String]): Unit =
    ServiceMain.mainThenExit(
      args, "Agent", AgentConfiguration.fromCommandLine(_), useLockFile = true
    )((conf, scheduler) =>
      for {
        agent <- RunningAgent.restartable(conf)(scheduler)
        iox <- IOExecutor.resource[Task](conf.config, conf.name)
        _ <- agent.webServer.restartWhenHttpsChanges(iox)
      } yield agent)
}
