package com.sos.scheduler.engine.common.configutils

import com.sos.scheduler.engine.common.convert.ConvertiblePartialFunction
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object Configs {
  val AllowMissing = ConfigParseOptions.defaults.setAllowMissing(true)

  def parseConfigIfExists(file: Path): Config = ConfigFactory.parseFile(file, AllowMissing)

  def parseConfigIfExists(file: Option[Path]): Config = file map parseConfigIfExists getOrElse ConfigFactory.empty

  implicit class ConvertibleConfig(val delegate: Config) extends ConvertiblePartialFunction[String, String] {
    def isDefinedAt(path: String) = delegate.hasPath(path)
    def apply(path: String): String = delegate.getString(path)
  }
}
