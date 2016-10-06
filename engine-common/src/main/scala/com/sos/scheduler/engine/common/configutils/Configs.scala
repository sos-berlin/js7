package com.sos.scheduler.engine.common.configutils

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions.wrappedConvert
import com.sos.scheduler.engine.base.convert.{As, ConvertiblePartialFunction}
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.ClassLoaders.currentClassLoader
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions, ConfigResolveOptions}
import java.nio.file.Files.exists
import java.nio.file.Path
import java.time.Duration
import scala.collection.JavaConversions._
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
object Configs {
  private val Required = ConfigParseOptions.defaults.setAllowMissing(false)
  private val logger = Logger(getClass)

  def parseConfigIfExists(file: Option[Path]): Config = file map parseConfigIfExists getOrElse ConfigFactory.empty

  def parseConfigIfExists(file: Path): Config = {
    if (exists(file)) {
      logger.info(s"Reading configuration file $file")
      ConfigFactory.parseFile(file, Required)
    } else {
      logger.trace(s"No configuration file $file")
      ConfigFactory.empty
    }
  }

  def loadResource(resource: JavaResource) = {
    logger.trace(s"Reading configuration JavaResource $resource")
    ConfigFactory.load(resource.path, Required.setClassLoader(currentClassLoader), ConfigResolveOptions.defaults)
  }

  implicit class ConvertibleConfig(val delegate: Config) extends ConvertiblePartialFunction[String, String] {
    def isDefinedAt(path: String) = delegate.hasPath(path)

    def apply(path: String): String = delegate.getString(path)

    def seqAs[W](path: String, default: ⇒ Iterable[W])(implicit convert: As[String, W]): immutable.Seq[W] =
      if (delegate.hasPath(path)) seqAs(path)(convert) else default.toVector

    def seqAs[W](path: String)(implicit convert: As[String, W]): immutable.Seq[W] =
      stringSeq(path) map wrappedConvert(convert, path)

    def stringSeq(path: String, default: ⇒ Iterable[String]): immutable.IndexedSeq[String] =
      if (delegate.hasPath(path)) stringSeq(path) else default.toVector

    def stringSeq(path: String): immutable.IndexedSeq[String] = delegate.getStringList(path).toVector


    def durationOption(path: String): Option[Duration] = delegate.hasPath(path) option delegate.getDuration(path)
  }
}
