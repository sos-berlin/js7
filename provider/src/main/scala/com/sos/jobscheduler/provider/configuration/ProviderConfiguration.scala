package com.sos.jobscheduler.provider.configuration

import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs.parseConfigIfExists
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final case class ProviderConfiguration(
  configDirectory: Path,
  masterUri: Uri,
  config: Config = ConfigFactory.empty)
{
  val liveDirectory = configDirectory / "live"
  val orderGeneratorsDirectory = configDirectory / "order-generators"

  // Suppresses Config (which may contain secrets)
  override def toString = s"ProviderConfiguration($configDirectory,$masterUri,Config)"
}

object ProviderConfiguration
{
  private lazy val DefaultConfigResource = JavaResource("com/sos/jobscheduler/provider/configuration/provider.conf")

  def fromCommandLine(args: Seq[String], addConfig: Config = ConfigFactory.empty): ProviderConfiguration =
    CommandLineArguments.parse(args) { a =>
      val configDir = a.as[Path]("-config-directory=").toAbsolutePath
      val config = ConfigFactory.parseMap(Map(
          "jobscheduler.config-directory" -> configDir.toString
        ).asJava)
        .withFallback(ConfigFactory.systemProperties)
        .withFallback(addConfig)
        .withFallback(parseConfigIfExists(configDir / "private" / "private.conf", secret = true))
        .withFallback(parseConfigIfExists(configDir / "provider.conf", secret = false))
        .withFallback(Configs.loadResource(DefaultConfigResource, internal = true))
        .resolve
      new ProviderConfiguration(
        configDirectory = configDir,
        masterUri = a.optionAs[Uri]("-master-uri=") getOrElse Uri(config.getString("jobscheduler.provider.master.uri")),
        config = config)
    }
}
