package js7.provider.configuration

import com.typesafe.config.ConfigFactory
import js7.base.web.Uri
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProviderConfigurationTest extends AnyFreeSpec
{
  "Empty command line" in {
    intercept[NoSuchElementException] { ProviderConfiguration.fromCommandLine(Nil) }
  }

  "Command line only" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      assert(ProviderConfiguration.fromCommandLine(
        s"-config-directory=$dir" ::
        "-controller-uri=http://example.com" :: Nil
      ).copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, Uri("http://example.com")))
    }
  }

  "Command line with provider.conf" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      dir / "provider.conf" := """js7.provider.controller.uri = "http://example.com"""" + "\n"
      assert(ProviderConfiguration.fromCommandLine(
        s"-config-directory=$dir" ::Nil
      ).copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, Uri("http://example.com")))
    }
  }
}
