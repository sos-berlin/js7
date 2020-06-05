package js7.master.configuration

import akka.util.Timeout
import js7.base.problem.Checked._
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configutils.Configs
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.time.JavaTimeConverters._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.common.utils.Tests.isTest
import js7.core.configuration.CommonConfiguration
import js7.core.event.journal.JournalConf
import js7.core.event.journal.data.JournalMeta
import js7.data.master.MasterId
import js7.master.cluster.ClusterConf
import js7.master.data.MasterSnapshots.SnapshotJsonCodec
import js7.master.data.events.MasterKeyedEventJsonCodec
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import java.time.ZoneId
import scala.jdk.CollectionConverters._
/**
  * @author Joacim Zschimmer
  */
final case class MasterConfiguration(
  masterId: MasterId,
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
  private def withCommandLineArguments(a: CommandLineArguments): MasterConfiguration =
    copy(masterId = a.as("-id=", masterId))

  def fileBasedDirectory: Path = configDirectory / "live"

  def stateDirectory: Path = dataDirectory / "state"

  lazy val journalMeta = JournalMeta(SnapshotJsonCodec, MasterKeyedEventJsonCodec, stateDirectory / "master")

  // Suppresses Config (which may contain secrets)
  override def toString = s"MasterConfiguration($masterId,$dataDirectory,$configDirectory,$webServerPorts," +
    s"$timeZone,$journalConf,$clusterConf,$name,Config)"
}

object MasterConfiguration
{
  val DefaultName = if (isTest) "Master" else "JS7"

  def forTest(configAndData: Path,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    mutualHttps: Boolean = false,
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
        httpsPort.map(o => WebServerPort.Https(new InetSocketAddress("127.0.0.1", o), mutual = mutualHttps)).toList)
  }

  lazy val DefaultConfig = Configs.loadResource(
    JavaResource("js7/master/configuration/master.conf"),
    internal = true)

  def fromCommandLine(commandLineArguments: CommandLineArguments, config: Config = ConfigFactory.empty) = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(commandLineArguments)
    val conf = fromDirectories(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      config,
      name = MasterConfiguration.DefaultName)
    conf.copy(webServerPorts = common.webServerPorts ++ conf.webServerPorts)
      .withCommandLineArguments(commandLineArguments)
  }

  private def fromDirectories(
    configDirectory: Path,
    dataDirectory: Path,
    extraDefaultConfig: Config,
    name: String
  ): MasterConfiguration = {
    val dataDir = dataDirectory.toAbsolutePath
    val configDir = configDirectory.toAbsolutePath
    val config = resolvedConfig(configDir, dataDir, extraDefaultConfig)
    val masterId = MasterId(config.getString("js7.master.id"))
    new MasterConfiguration(
      masterId = masterId,
      dataDirectory = dataDir,
      configDirectory = configDir,
      webServerPorts = Nil,
        //config.seqAs("js7.webserver.http.ports")(StringToServerInetSocketAddress) map WebServerBinding.Http,
      timeZone = ZoneId.systemDefault,
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
      journalConf = JournalConf.fromConfig(config),
      clusterConf = ClusterConf.fromConfig(masterId.toUserId, config).orThrow,
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

  // Same code in AkkaHttpMasterTextApi.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    parseConfigIfExists(configDirectory / "private/private.conf", secret = true)
      .withFallback(parseConfigIfExists(configDirectory / "master.conf", secret = false))
}
