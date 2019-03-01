package com.sos.jobscheduler.provider.configuration

import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs.parseConfigIfExists
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class ProviderConfiguration(
  configDirectory: Path,
  masterUri: String,
  config: Config = ConfigFactory.empty)
{
  val liveDirectory = configDirectory / "live"
  val orderGeneratorsDirectory = configDirectory / "order-generators"
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
        .withFallback(parseConfigIfExists(configDir / "private" / "private.conf"))
        .withFallback(parseConfigIfExists(configDir / "provider.conf"))
        .withFallback(Configs.loadResource(DefaultConfigResource))
        .resolve
      new ProviderConfiguration(
        configDirectory = configDir,
        masterUri = a.optionAs[String]("-master-uri=") getOrElse config.getString("jobscheduler.provider.master.uri"),
        config = config)
    }
}
