package js7.provider

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import js7.common.system.startup.ServiceApp
import js7.provider.configuration.ProviderConfiguration

object ProviderMain extends ServiceApp:
  // No Logger here!

  def run(args: List[String]): IO[ExitCode] =
    given IORuntime = runtime
    runService(args, ProviderConfiguration.fromCommandLine(_)):
      Provider.resource
