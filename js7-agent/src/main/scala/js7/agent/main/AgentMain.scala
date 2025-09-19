package js7.agent.main

import cats.effect.{ExitCode, IO}
import js7.agent.configuration.AgentConfiguration
import js7.agent.{RestartableDirector, RunningAgent}
import js7.common.system.startup.ServiceApp

object AgentMain extends ServiceApp:
  // No Logger here!

  override protected val productName = "Agent"

  def run(args: List[String]): IO[ExitCode] =
    runService(args, AgentConfiguration.fromCommandLine(_), useLockFile = true):
      conf =>
        for
          restartableDirector <- RunningAgent.restartableDirector(conf)(using runtime)
          _ <- restartableDirector.webServer.restartWhenHttpsChanges
        yield
          restartableDirector
