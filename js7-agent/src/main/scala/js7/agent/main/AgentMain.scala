package js7.agent.main

import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO}
import js7.agent.configuration.AgentConfiguration
import js7.agent.{RestartableDirector, RunningAgent}
import js7.base.catsutils.Environment.environment
import js7.base.thread.IOExecutor
import js7.common.system.startup.ServiceApp

object AgentMain extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    runService(args, "JS7 Agent", AgentConfiguration.fromCommandLine(_), useLockFile = true):
      conf =>
        for
          agent <- RunningAgent.restartable(conf)(using runtime)
          given IOExecutor <- Resource.eval(environment[IOExecutor])
          _ <- agent.webServer.restartWhenHttpsChanges
        yield
          agent
