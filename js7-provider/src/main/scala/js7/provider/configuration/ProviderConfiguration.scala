package js7.provider.configuration

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.configutils.Configs
import js7.base.configutils.Configs.parseConfigIfExists
import js7.base.convert.AsJava.StringAsPath
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.https.HttpsConfig
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.JobSchedulerConfiguration
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final case class ProviderConfiguration(
  configDirectory: Path,
  controllerUri: Uri,
  httpsConfig: HttpsConfig,
  config: Config = ConfigFactory.empty)
{
  val liveDirectory = configDirectory / "live"
  val orderGeneratorsDirectory = configDirectory / "order-generators"

  // Suppresses Config (which may contain secrets)
  override def toString = s"ProviderConfiguration($configDirectory,$controllerUri,Config)"
}

object ProviderConfiguration
{
  private val DefaultConfigResource = JavaResource("js7/provider/configuration/provider.conf")

  def fromCommandLine(args: Seq[String], addConfig: Config = ConfigFactory.empty): ProviderConfiguration =
    CommandLineArguments.parse(args) { a =>
      val configDir = a.as[Path]("--config-directory=").toAbsolutePath
      val config = ConfigFactory.parseMap(Map(
          "js7.config-directory" -> configDir.toString
        ).asJava)
        .withFallback(ConfigFactory.systemProperties)
        .withFallback(addConfig)
        .withFallback(parseConfigIfExists(configDir / "private" / "private.conf", secret = true))
        .withFallback(parseConfigIfExists(configDir / "provider.conf", secret = false))
        .withFallback(JobSchedulerConfiguration.defaultConfig)
        .withFallback(Configs.loadResource(DefaultConfigResource))
        .resolve
      new ProviderConfiguration(
        configDirectory = configDir,
        controllerUri = a.optionAs[Uri]("--controller-uri=") getOrElse Uri(config.getString("js7.provider.controller.uri")),
        HttpsConfig.fromConfig(config, configDir),
        config = config)
    }
}
