package js7.agent.main

import cats.effect.{ExitCode, IO}
import js7.agent.configuration.AgentConfiguration
import js7.agent.{RestartableDirector, RunningAgent}
import js7.base.thread.IOExecutor
import js7.common.system.startup.ServiceApp

object AgentMain extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    runService(args, "JS7 Agent", AgentConfiguration.fromCommandLine(_), useLockFile = true):
      conf =>
        for
          agent <- RunningAgent.restartable(conf)(using runtime)
          iox <- IOExecutor.resource[IO](conf.config, conf.name + " I/O")
          _ <- agent.webServer.restartWhenHttpsChanges(using iox)
        yield
          agent
