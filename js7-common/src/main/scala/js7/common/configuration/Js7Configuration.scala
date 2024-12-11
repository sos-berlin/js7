package js7.common.configuration

import com.typesafe.config.{Config, ConfigFactory}
import js7.base.BuildInfo
import js7.base.configutils.Configs
import js7.base.io.JavaResource
import js7.base.utils.Tests.isTest
import scala.jdk.CollectionConverters.*

object Js7Configuration:
  val defaultConfig: Config =
    val map = Map(
      "js7.version" -> BuildInfo.version,
      "js7.longVersion" -> BuildInfo.longVersion,
      "js7.prettyVersion" -> BuildInfo.prettyVersion,
      "js7.test" -> isTest.toString)
    ConfigFactory
      .parseMap(map.asJava)
      .withFallback(
        Configs.loadResource(
          JavaResource("js7/common/configuration/js7.conf")))
      .resolve()
