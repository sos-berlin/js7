package js7.provider.configuration

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.configutils.Configs
import js7.base.configutils.Configs.parseConfigIfExists
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{CommonConfiguration, Js7Configuration}
import js7.common.pekkohttp.web.data.WebServerPort
import scala.jdk.CollectionConverters.*
import js7.base.convert.AsJava.StringAsPath

/**
  * @author Joacim Zschimmer
  */
final case class ProviderConfiguration(
  configDirectory: Path,
  controllerUri: Uri,
  webServerPorts: Seq[WebServerPort] = Nil,
  testSuppressStart: Boolean = false,
  config: Config = ConfigFactory.empty)
extends CommonConfiguration:
  val name = "Provider"

  val liveDirectory: Path = configDirectory / "live"
  val orderGeneratorsDirectory: Path = configDirectory / "order-generators"

  // Suppresses Config (which may contain secrets)
  override def toString = s"ProviderConfiguration($configDirectory,$controllerUri,Config)"


object ProviderConfiguration:
  private val DefaultConfigResource = JavaResource("js7/provider/configuration/provider.conf")

  def fromCommandLine(a: CommandLineArguments, addConfig: Config = ConfigFactory.empty)
  : ProviderConfiguration =
    val common = CommonConfiguration.Common.fromCommandLineArguments(a)
    import common.configDirectory
    val configDir = a.as[Path]("--config-directory=").toAbsolutePath
    a.as[Path]("--data-directory=").toAbsolutePath // not used
    val config = ConfigFactory.parseMap(Map(
      "js7.config-directory" -> configDirectory.toString
    ).asJava)
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(addConfig)
      .withFallback(parseConfigIfExists(configDirectory / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "provider.conf", secret = false))
      .withFallback(Js7Configuration.defaultConfig)
      .withFallback(Configs.loadResource(DefaultConfigResource))
      .resolve
    new ProviderConfiguration(
      configDirectory = configDirectory,
      controllerUri = a.optionAs[Uri]("--controller-uri=")
        .getOrElse(Uri(config.getString("js7.provider.controller.uri"))),
      common.webServerPorts,
      config = config)
