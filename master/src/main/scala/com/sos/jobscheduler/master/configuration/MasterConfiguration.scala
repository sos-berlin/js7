package com.sos.jobscheduler.master.configuration

import akka.util.Timeout
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.common.utils.Tests.isTest
import com.sos.jobscheduler.core.configuration.CommonConfiguration
import com.sos.jobscheduler.core.event.journal.JournalConf
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.cluster.ClusterConf
import com.sos.jobscheduler.master.data.MasterSnapshots.SnapshotJsonCodec
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import java.time.ZoneId
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

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
  val DefaultName = if (isTest) "Master" else "JobScheduler"

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
        httpPort.map(o => WebServerPort.Http(new InetSocketAddress("127.0.0.1", o))) ++:
        httpsPort.map(o => WebServerPort.Https(new InetSocketAddress("127.0.0.1", o), mutual = mutualHttps)).toList)
  }

  lazy val DefaultConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/master/configuration/master.conf"),
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
    val config = resolvedConfig(configDir, extraDefaultConfig)
    val masterId = MasterId(config.getString("jobscheduler.master.id"))
    new MasterConfiguration(
      masterId = masterId,
      dataDirectory = dataDir,
      configDirectory = configDir,
      webServerPorts = Nil,
        //config.seqAs("jobscheduler.webserver.http.ports")(StringToServerInetSocketAddress) map WebServerBinding.Http,
      timeZone = ZoneId.systemDefault,
      akkaAskTimeout = config.getDuration("jobscheduler.akka.ask-timeout").toFiniteDuration,
      journalConf = JournalConf.fromConfig(config),
      clusterConf = ClusterConf.fromConfigAndFile(masterId.toUserId, config).orThrow,
      name = name,
      config = config)
  }

  private def resolvedConfig(configDirectory: Path, extraDefaultConfig: Config): Config = {
    val config = configDirectoryConfig(configDirectory)
    ConfigFactory.parseMap(Map(
        "jobscheduler.config-directory" -> configDirectory.toString
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
