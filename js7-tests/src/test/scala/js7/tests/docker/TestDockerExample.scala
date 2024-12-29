package js7.tests.docker

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp

object TestDockerExample extends ServiceApp:

  private given IORuntime = runtime

  def run(args: List[String]): IO[ExitCode] =
    runService(args, _ => BasicConfiguration.Empty): _ =>
      programAsService:
        run

  private def run: IO[ExitCode] =
    TestDockerEnvironment.temporaryResource(dontCleanUp = true).use:
      _.run
