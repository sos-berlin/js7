package js7.provider.configuration

import com.typesafe.config.ConfigFactory
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.https.HttpsConfig
import js7.base.test.Test
import js7.base.web.Uri

/**
  * @author Joacim Zschimmer
  */
final class ProviderConfigurationTest extends Test
{
  "Empty command line" in {
    intercept[NoSuchElementException] { ProviderConfiguration.fromCommandLine(Nil) }
  }

  "Command line only" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      assert(ProviderConfiguration.fromCommandLine(
        s"--config-directory=$dir" ::
        "--controller-uri=https://example.com" :: Nil
      ).copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, Uri("https://example.com"), HttpsConfig.empty))
    }
  }

  "Command line with provider.conf" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      dir / "provider.conf" := """js7.provider.controller.uri = "https://example.com"""" + "\n"
      assert(ProviderConfiguration.fromCommandLine(
        s"--config-directory=$dir" ::Nil
      ).copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, Uri("https://example.com"), HttpsConfig.empty))
    }
  }
}
