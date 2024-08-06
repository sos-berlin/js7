package js7.agent.main

import cats.effect.{ExitCode, IO}
import js7.agent.configuration.AgentConfiguration
import js7.agent.{RestartableDirector, RunningAgent}
import js7.common.system.startup.ServiceApp

object AgentMain extends ServiceApp:
  // No Logger here!

  override protected def productName = "JS7 Agent"

  def run(args: List[String]): IO[ExitCode] =
    runService(args, AgentConfiguration.fromCommandLine(_), useLockFile = true):
      conf =>
        for
          agent <- RunningAgent.restartable(conf)(using runtime)
          _ <- agent.webServer.restartWhenHttpsChanges
        yield
          agent
