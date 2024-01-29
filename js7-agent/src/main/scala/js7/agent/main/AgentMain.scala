package js7.agent.main

import cats.effect.{ExitCode, IO, IOApp}
import js7.agent.{RestartableDirector, RunningAgent}
import js7.agent.configuration.AgentConfiguration
import js7.base.thread.IOExecutor
import js7.base.utils.ProgramTermination
import js7.common.system.startup.ServiceMain

object AgentMain extends IOApp:
  // No Logger here!

  def run(args: List[String]) =
    run2(args)(_.untilTerminated)

  def run2(args: List[String])(use: RestartableDirector => IO[ProgramTermination]): IO[ExitCode] =
    ServiceMain
      .runAsMain(args, "JS7 Agent", AgentConfiguration.fromCommandLine(_), useLockFile = true)(
        conf =>
          for
            agent <- RunningAgent.restartable(conf)(using runtime)
            iox <- IOExecutor.resource[IO](conf.config, conf.name)
            _ <- agent.webServer.restartWhenHttpsChanges(iox)
          yield
            agent,
        use = (_, service: RestartableDirector) => use(service))
