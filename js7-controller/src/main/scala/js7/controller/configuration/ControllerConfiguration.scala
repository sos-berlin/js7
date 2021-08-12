package js7.controller.configuration

import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import java.time.ZoneId
import js7.base.configutils.Configs
import js7.base.configutils.Configs._
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.time.JavaTimeConverters._
import js7.cluster.ClusterConf
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.JobSchedulerConfiguration
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.Tests.isTest
import js7.core.configuration.CommonConfiguration
import js7.data.controller.{ControllerId, ControllerState}
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final case class ControllerConfiguration(
  controllerId: ControllerId,
  dataDirectory: Path,
  configDirectory: Path,
  webServerPorts: Seq[WebServerPort],
  timeZone: ZoneId,
  implicit val akkaAskTimeout: Timeout,
  journalConf: JournalConf,
  clusterConf: ClusterConf,
  name: String,
  config: Config)
extends CommonConfiguration
{
  def itemDirectory: Path = configDirectory / "live"

  def stateDirectory: Path = dataDirectory / "state"

  lazy val journalMeta = JournalMeta(ControllerState, stateDirectory / "controller")

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
    .withFallback(JobSchedulerConfiguration.defaultConfig)

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
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
      journalConf = JournalConf.fromConfig(config),
      clusterConf = ClusterConf.fromConfig(controllerId.toUserId, config).orThrow,
      name = name,
      config = config)
  }

  private def resolvedConfig(configDirectory: Path, dataDirectory: Path, extraDefaultConfig: Config): Config = {
    val config = configDirectoryConfig(configDirectory)
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

  // Same code in AkkaHttpControllerTextApi.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    parseConfigIfExists(configDirectory / "private/private.conf", secret = true)
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))
}
