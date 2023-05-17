package js7.provider

import js7.common.system.startup.ServiceMain
import js7.provider.configuration.ProviderConfiguration

object ProviderMain
{
  // No Logger here!

  def main(args: Array[String]): Unit =
    ServiceMain.mainThenExit(args, "Provider", ProviderConfiguration.fromCommandLine(_))(
      Provider.resource(_)(_))
}
