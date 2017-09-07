package com.sos.jobscheduler.master.configuration

import akka.util.Timeout
import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.convert.As._
import com.sos.jobscheduler.common.akkahttp.WebServerBinding
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.internet.IP.StringToServerInetSocketAddress
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
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
  dataDirectory: Path,
  configDirectoryOption: Option[Path],
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

  def liveDirectoryOption: Option[Path] = configDirectoryOption map { _ / "live" }

  def stateDirectory: Path = dataDirectory / "state"
}

object MasterConfiguration {

  def forTest(configAndData: Path, httpPort: Int = findRandomFreeTcpPort(), config: Config = ConfigFactory.empty) =
    fromDataDirectory(dataDirectory = configAndData / "config", configDirectory = Some(configAndData / "config"), config).copy(
      webServerBindings = Vector(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort))))

  private[configuration] lazy val DefaultConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/master/configuration/master.conf"))

  def fromCommandLine(args: Seq[String]) = CommandLineArguments.parse(args) { a ⇒
    fromDataDirectory(
      dataDirectory = a.as[Path]("-data-directory="),
      configDirectory = a.optionAs[Path]("-config-directory=")
    ) withCommandLineArguments a
  }

  def fromDataDirectory(dataDirectory: Path, configDirectory: Option[Path], extraDefaultConfig: Config = ConfigFactory.empty): MasterConfiguration = {
    val dataDir = dataDirectory.toAbsolutePath
    val configDir = configDirectory map { _.toAbsolutePath }
    val config = resolvedConfig(configDir, extraDefaultConfig)
    val masterConfig = config.getConfig("jobscheduler.master")
    new MasterConfiguration(
      dataDirectory = dataDir,
      configDirectoryOption = configDir,
      webServerBindings = Vector[WebServerBinding]() ++
        (masterConfig.optionAs("webserver.http.port")(StringToServerInetSocketAddress) map WebServerBinding.Http),
      timeZone = ZoneId.systemDefault,
      akkaAskTimeout = masterConfig.getDuration("akka-ask-timeout").toFiniteDuration,
      journalSyncOnCommit = masterConfig.getBoolean("journal.sync"),
      config = config)
  }

  private def resolvedConfig(configDirectory: Option[Path], extraDefaultConfig: Config): Config = {
    val config = configDirectory map configDirectoryConfig getOrElse ConfigFactory.empty
    (config withFallback extraDefaultConfig withFallback DefaultConfig).resolve
  }

  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf"))
}
