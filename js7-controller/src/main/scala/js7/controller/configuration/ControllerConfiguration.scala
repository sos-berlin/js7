package js7.controller.configuration

import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import java.time.ZoneId
import js7.base.configutils.Configs
import js7.base.configutils.Configs.*
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.time.JavaTimeConverters.*
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterConf
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{BasicConfiguration, CommonConfiguration, Js7Configuration}
import js7.common.pekkohttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.controller.{ControllerId, ControllerState}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import org.apache.pekko.util.Timeout
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final case class ControllerConfiguration(
  controllerId: ControllerId,
  dataDirectory: Path,
  configDirectory: Path,
  webServerPorts: Seq[WebServerPort],
  timeZone: ZoneId,
  pekkoAskTimeout: Timeout,
  clusterConf: ClusterConf,
  name: String,
  config: Config)
extends BasicConfiguration with CommonConfiguration
{
  override def maybeConfigDirectory = Some(configDirectory)

  override def maybeDataDirectory = Some(dataDirectory)

  implicit def implicitPekkoAskTimeout: Timeout = pekkoAskTimeout

  def stateDirectory: Path = dataDirectory / "state"

  def workDirectory: Path = dataDirectory / "work"

  lazy val journalLocation = JournalLocation(ControllerState, stateDirectory / "controller")

  def journalConf: JournalConf =
    clusterConf.journalConf

  // Suppresses Config (which may contain secrets)
  override def toString = s"ControllerConfiguration($controllerId,$dataDirectory,$configDirectory,$webServerPorts," +
    s"$timeZone,$journalConf,$clusterConf,$name,Config)"
}

object ControllerConfiguration
{
  val DefaultName = if (isTest) "Controller" else "JS7"

  def forTest(configAndData: Path,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    name: String = DefaultName
  ) = {
    val data = configAndData / "data"
    if (!Files.exists(data)) createDirectory(data)
    val state = data / "state"
    if (!Files.exists(state)) createDirectory(state)
    val work = data / "work"
    if (!Files.exists(work)) createDirectory(work)
    fromDirectories(
      configDirectory = configAndData / "config",
      dataDirectory = data,
      config,
      name = name)
    .copy(
      webServerPorts =
        httpPort.map(o => WebServerPort.localhost(o)) ++:
        httpsPort.map(o => WebServerPort.Https(new InetSocketAddress("127.0.0.1", o)))
          .toList)
  }

  lazy val DefaultConfig = Configs
    .loadResource(JavaResource("js7/controller/configuration/controller.conf"))
    .withFallback(Js7Configuration.defaultConfig)

  def fromCommandLine(commandLineArguments: CommandLineArguments, config: Config = ConfigFactory.empty) = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(commandLineArguments)
    val conf = fromDirectories(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      config,
      commandLineArguments.optionAs[ControllerId]("--id="),
      name = ControllerConfiguration.DefaultName)
    conf.copy(webServerPorts = common.webServerPorts ++ conf.webServerPorts)
  }

  private def fromDirectories(
    configDirectory: Path,
    dataDirectory: Path,
    extraDefaultConfig: Config,
    maybeControllerId: Option[ControllerId] = None,
    name: String)
  : ControllerConfiguration = {
    val dataDir = dataDirectory.toAbsolutePath
    val configDir = configDirectory.toAbsolutePath
    val config = resolvedConfig(configDir, dataDir, extraDefaultConfig)
    val controllerId = maybeControllerId getOrElse ControllerId(config.getString("js7.controller.id"))
    new ControllerConfiguration(
      controllerId = controllerId,
      dataDirectory = dataDir,
      configDirectory = configDir,
      webServerPorts = Nil,
      timeZone = ZoneId.systemDefault,
      pekkoAskTimeout = config.getDuration("js7.pekko.ask-timeout").toFiniteDuration,
      clusterConf = ClusterConf.fromConfig(config).orThrow,
      name = name,
      config = config)
  }

  private def resolvedConfig(configDirectory: Path, dataDirectory: Path, extraDefaultConfig: Config): Config = {
    val config = configDirectoryToConfig(configDirectory)
    ConfigFactory.parseMap(Map(
        "js7.config-directory" -> configDirectory.toString,
        "js7.data-directory" -> dataDirectory.toString
      ).asJava)
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(config)
      .withFallback(extraDefaultConfig)
      .withFallback(DefaultConfig)
      .resolve()
  }

  // Same code in PekkoHttpControllerTextApi.configDirectoryToConfig
  private def configDirectoryToConfig(configDirectory: Path): Config =
    parseConfigIfExists(configDirectory / "private/private.conf", secret = true)
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))
}
