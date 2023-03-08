package js7.provider.configuration

import com.typesafe.config.ConfigFactory
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.https.HttpsConfig
import js7.base.test.OurTestSuite
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments

/**
  * @author Joacim Zschimmer
  */
final class ProviderConfigurationTest extends OurTestSuite
{
  "Empty command line" in {
    intercept[NoSuchElementException] {
      ProviderConfiguration.fromCommandLine(CommandLineArguments(Nil))
    }
  }

  "Command line only" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      assert(CommandLineArguments
        .parse(Seq(
          s"--config-directory=$dir",
          "--controller-uri=https://example.com")
        )(ProviderConfiguration.fromCommandLine(_))
        .copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, Uri("https://example.com"), HttpsConfig.empty))
    }
  }

  "Command line with provider.conf" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      dir / "provider.conf" := """js7.provider.controller.uri = "https://example.com"""" + "\n"
      assert(
        CommandLineArguments
          .parse(Seq(
            s"--config-directory=$dir")
          )(ProviderConfiguration.fromCommandLine(_))
          .copy(config = ConfigFactory.empty)
          == ProviderConfiguration(dir, Uri("https://example.com"), HttpsConfig.empty))
    }
  }
}
