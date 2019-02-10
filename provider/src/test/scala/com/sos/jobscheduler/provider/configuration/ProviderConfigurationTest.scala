package com.sos.jobscheduler.provider.configuration

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProviderConfigurationTest extends FreeSpec
{
  "Empty command line" in {
    intercept[NoSuchElementException] { ProviderConfiguration.fromCommandLine(Nil) }
  }

  "Command line only" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      assert(ProviderConfiguration.fromCommandLine(
        s"-config-directory=$dir" ::
        "-master-uri=http://example.com" :: Nil
      ).copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, "http://example.com"))
    }
  }

  "Command line with provider.conf" in {
    withTemporaryDirectory("ProviderConfigurationTest-") { dir =>
      dir / "provider.conf" := """jobscheduler.provider.master.uri = "http://example.com"""" + "\n"
      assert(ProviderConfiguration.fromCommandLine(
        s"-config-directory=$dir" ::Nil
      ).copy(config = ConfigFactory.empty)
        == ProviderConfiguration(dir, "http://example.com"))
    }
  }
}
