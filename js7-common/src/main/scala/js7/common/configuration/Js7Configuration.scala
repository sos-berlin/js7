package js7.common.configuration

import com.typesafe.config.{Config, ConfigFactory}
import js7.base.BuildInfo
import js7.base.configutils.Configs
import js7.base.io.JavaResource
import scala.jdk.CollectionConverters.*

object Js7Configuration
{
  val defaultConfig: Config = {
    val map = Map(
      "js7.version" -> BuildInfo.version,
      "js7.longVersion" -> BuildInfo.longVersion,
      "js7.prettyVersion" -> BuildInfo.prettyVersion,
      "js7.test" -> (
        if (sys.props.contains("test.speed"))
          "off"
        else
          sys.props.getOrElse("js7.test", "off")))
    ConfigFactory
      .parseMap(map.asJava)
      .withFallback(
        Configs.loadResource(
          JavaResource("js7/common/configuration/js7.conf")))
      .resolve()
  }
}
