package com.sos.jobscheduler.master.configuration

import akka.util.Timeout
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.configuration.CommonConfiguration
import com.sos.jobscheduler.data.master.MasterId
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Path
import java.time.ZoneId
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
  name: String,
  config: Config)
extends CommonConfiguration
{
  private def withCommandLineArguments(a: CommandLineArguments): MasterConfiguration =
    copy(masterId = a.as("-id=", masterId))

  def fileBasedDirectory: Path = configDirectory / "live"

  def orderGeneratorsDirectory: Path = configDirectory / "order-generators"

  def stateDirectory: Path = dataDirectory / "state"
}

object MasterConfiguration
{
  val DefaultName = "Master"

  def forTest(configAndData: Path,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findRandomFreeTcpPort()),
    httpsPort: Option[Int] = None,
    mutualHttps: Boolean = false,
    name: String = DefaultName
  ) =
    fromDirectories(
      configDirectory = configAndData / "config",
      dataDirectory = configAndData / "data",
      config,
      name = name)
    .copy(
      webServerPorts =
        httpPort.map(o ⇒ WebServerPort.Http(new InetSocketAddress("127.0.0.1", o))) ++:
        httpsPort.map(o ⇒ WebServerPort.Https(new InetSocketAddress("127.0.0.1", o), mutual = mutualHttps)).toList)

  lazy val DefaultConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/master/configuration/master.conf"))

  def fromCommandLine(args: Seq[String], config: Config = ConfigFactory.empty) =
    CommandLineArguments.parse(args) { a ⇒
      val common = CommonConfiguration.Common.fromCommandLineArguments(a)
      val conf = fromDirectories(
        configDirectory = common.configDirectory,
        dataDirectory = common.dataDirectory,
        config,
        name = MasterConfiguration.DefaultName)
      conf.copy(webServerPorts = common.webServerPorts ++ conf.webServerPorts)
        .withCommandLineArguments(a)
    }

  private def fromDirectories(
    configDirectory: Path,
    dataDirectory: Path,
    extraDefaultConfig: Config = ConfigFactory.empty,
    name: String
  ): MasterConfiguration = {
    val dataDir = dataDirectory.toAbsolutePath
    val configDir = configDirectory.toAbsolutePath
    val config = resolvedConfig(configDir, extraDefaultConfig)
    new MasterConfiguration(
      masterId = MasterId(config.getString("jobscheduler.master.id")),
      dataDirectory = dataDir,
      configDirectory = configDir,
      webServerPorts = Nil,
        //config.seqAs("jobscheduler.webserver.http.ports")(StringToServerInetSocketAddress) map WebServerBinding.Http,
      timeZone = ZoneId.systemDefault,
      akkaAskTimeout = config.getDuration("jobscheduler.akka-ask-timeout").toFiniteDuration,
      name = name,
      config = config)
  }

  private def resolvedConfig(configDirectory: Path, extraDefaultConfig: Config): Config = {
    val config = configDirectoryConfig(configDirectory)
    ConfigFactory.systemProperties
      .withFallback(config)
      .withFallback(extraDefaultConfig)
      .withFallback(DefaultConfig)
      .resolve
  }

  // Same code in AkkaHttpMasterTextApi.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf"))
}
