package com.sos.jobscheduler.master.configuration

import akka.util.Timeout
import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.convert.As._
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs.{parseConfigIfExists, _}
import com.sos.jobscheduler.common.internet.IP.StringToServerInetSocketAddress
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.sprayutils.WebServerBinding
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Path
import java.time.ZoneId
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class MasterConfiguration(
  dataDirectoryOption: Option[Path],
  webServerBindings: Vector[WebServerBinding],
  timeZone: ZoneId,
  implicit val akkaAskTimeout: Timeout,
  journalSyncOnCommit: Boolean,
  config: Config)
{
  private def withCommandLineArguments(a: CommandLineArguments): MasterConfiguration =
    copy(
      webServerBindings = webServerBindings ++
        a.optionAs("-http-port=")(As(o ⇒ WebServerBinding.Http(StringToServerInetSocketAddress(o)))))

  def configDirectoryOption: Option[Path] = dataDirectoryOption map { _ / "config" }

  def liveDirectoryOption: Option[Path] = configDirectoryOption map { _ / "live" }

  def stateDirectoryOption: Option[Path] = dataDirectoryOption map { _ / "state" }
}

object MasterConfiguration {

  def forTest(data: Option[Path] = None, httpPort: Int = findRandomFreeTcpPort(), config: Config = ConfigFactory.empty) =
    fromDataDirectory(data, config).copy(
      webServerBindings = Vector(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort))))

  private[configuration] lazy val DefaultConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/master/configuration/master.conf"))

  def fromCommandLine(args: Seq[String]) = CommandLineArguments.parse(args) { a ⇒
    fromDataDirectory(a.optionAs[Path]("-data-directory=")) withCommandLineArguments a
  }

  def fromDataDirectory(dataDirectory: Option[Path], extraDefaultConfig: Config = ConfigFactory.empty): MasterConfiguration = {
    val data = dataDirectory map { _.toAbsolutePath }
    val config = resolvedConfig(data, extraDefaultConfig)
    val masterConfig = config.getConfig("jobscheduler.master")
    new MasterConfiguration(
      dataDirectoryOption = data,
      webServerBindings = Vector[WebServerBinding]() ++
        (masterConfig.optionAs("webserver.http.port")(StringToServerInetSocketAddress) map WebServerBinding.Http),
      timeZone = ZoneId.systemDefault,
      akkaAskTimeout = masterConfig.getDuration("akka-ask-timeout").toFiniteDuration,
      journalSyncOnCommit = masterConfig.getBoolean("journal.sync"),
      config = config)
  }

  private def resolvedConfig(dataDirectory: Option[Path], extraDefaultConfig: Config): Config = {
    val dataConfig = dataDirectory map dataDirectoryConfig getOrElse ConfigFactory.empty
    (dataConfig withFallback extraDefaultConfig withFallback DefaultConfig).resolve
  }

  private def dataDirectoryConfig(dataDirectory: Path): Config =
    ConfigFactory
      .empty  //.parseMap(Map("jobscheduler.master.data.directory" → dataDirectoryOption.toString))  // For substitution of ${jobscheduler.agent.data.directory}
      .withFallback(parseConfigIfExists(dataDirectory / "config/private/private.conf"))
      .withFallback(parseConfigIfExists(dataDirectory / "config/agent.conf"))
}
