package js7.common.configuration

import com.typesafe.config.{Config, ConfigFactory}
import js7.base.BuildInfo
import js7.base.configutils.Configs
import js7.base.io.JavaResource
import scala.jdk.CollectionConverters._

object JobSchedulerConfiguration
{
  val defaultConfig: Config =
    ConfigFactory
      .parseMap(
        Map(
          "js7.version" -> BuildInfo.version,
          "js7.longVersion" -> BuildInfo.longVersion
        ).asJava)
      .withFallback(
        Configs.loadResource(
          JavaResource("js7/common/configuration/js7.conf")))
      .resolve()
}
