package js7.provider.configuration

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.configutils.Configs
import js7.base.configutils.Configs.parseConfigIfExists
import js7.base.convert.AsJava.StringAsPath
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.HttpsConfig
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{BasicConfiguration, Js7Configuration}
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final case class ProviderConfiguration(
  configDirectory: Path,
  controllerUri: Uri,
  httpsConfig: HttpsConfig,
  testSuppressStart: Boolean = false,
  config: Config = ConfigFactory.empty)
extends BasicConfiguration:
  val name = "Provider"

  val liveDirectory: Path = configDirectory / "live"
  val orderGeneratorsDirectory: Path = configDirectory / "order-generators"

  // Suppresses Config (which may contain secrets)
  override def toString = s"ProviderConfiguration($configDirectory,$controllerUri,Config)"


object ProviderConfiguration:
  private val DefaultConfigResource = JavaResource("js7/provider/configuration/provider.conf")

  def fromCommandLine(a: CommandLineArguments, addConfig: Config = ConfigFactory.empty)
  : ProviderConfiguration =
    val configDir = a.as[Path]("--config-directory=").toAbsolutePath
    a.as[Path]("--data-directory=").toAbsolutePath // not used
    val config = ConfigFactory.parseMap(Map(
      "js7.config-directory" -> configDir.toString
    ).asJava)
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(addConfig)
      .withFallback(parseConfigIfExists(configDir / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDir / "provider.conf", secret = false))
      .withFallback(Js7Configuration.defaultConfig)
      .withFallback(Configs.loadResource(DefaultConfigResource))
      .resolve
    new ProviderConfiguration(
      configDirectory = configDir,
      controllerUri = a.optionAs[Uri]("--controller-uri=")
        .getOrElse(Uri(config.getString("js7.provider.controller.uri"))),
      HttpsConfig.fromConfig(config, configDir),
      config = config)
