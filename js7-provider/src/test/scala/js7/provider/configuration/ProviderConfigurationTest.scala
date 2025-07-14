package js7.provider.configuration

import com.typesafe.config.ConfigFactory
import java.nio.file.Files.createDirectory
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.test.OurTestSuite
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments

/**
  * @author Joacim Zschimmer
  */
final class ProviderConfigurationTest extends OurTestSuite:
  "Empty command line" in:
    intercept[NoSuchElementException]:
      ProviderConfiguration.fromCommandLine(CommandLineArguments(Nil))

  "Command line only" in:
    withTemporaryDirectory("ProviderConfigurationTest-"): dir =>
      val configDir = dir / "config"
      val dataDir = dir / "data"
      val args = Seq(
        s"--config-directory=$configDir",
        s"--data-directory=$dataDir",
        "--controller-uri=https://example.com")
      assert:
        CommandLineArguments.parse(args)(ProviderConfiguration.fromCommandLine(_))
          .copy(config = ConfigFactory.empty)
          == ProviderConfiguration(configDir, Uri("https://example.com"))

  "Command line with provider.conf" in:
    withTemporaryDirectory("ProviderConfigurationTest-"): dir =>
      val configDir = dir / "config"
      val dataDir = dir / "data"
      createDirectory(configDir)
      configDir / "provider.conf" := """js7.provider.controller.uri = "https://example.com"""" + "\n"
      val args = Seq(
        s"--config-directory=$configDir",
        s"--data-directory=$dataDir")
      assert:
        CommandLineArguments
          .parse(args)(ProviderConfiguration.fromCommandLine(_))
          .copy(config = ConfigFactory.empty)
          == ProviderConfiguration(configDir, Uri("https://example.com"))
