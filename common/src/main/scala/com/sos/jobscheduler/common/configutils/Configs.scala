package com.sos.jobscheduler.common.configutils

import com.sos.jobscheduler.base.convert.ConvertiblePartialFunctions.wrappedConvert
import com.sos.jobscheduler.base.convert.{As, ConvertiblePartialFunction}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.ClassLoaders.currentClassLoader
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions, ConfigResolveOptions}
import java.nio.file.Files.exists
import java.nio.file.Path
import java.time.Duration
import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
object Configs {
  private val Required = ConfigParseOptions.defaults.setAllowMissing(false)
  private val logger = Logger(getClass)

  def parseConfigIfExists(file: Option[Path]): Config = file.fold(ConfigFactory.empty)(parseConfigIfExists)

  def parseConfigIfExists(file: Path): Config =
    if (exists(file)) {
      logger.info(s"Reading configuration files $file")
      ConfigFactory.parseFile(file, Required)
    } else {
      logger.debug(s"No configuration files $file")
      ConfigFactory.empty
    }

  def loadResource(resource: JavaResource) = {
    logger.trace(s"Reading configuration JavaResource $resource")
    ConfigFactory.load(resource.path, Required.setClassLoader(currentClassLoader), ConfigResolveOptions.defaults)
  }

  implicit final class ConvertibleConfig(private val delegate: Config) extends ConvertiblePartialFunction[String, String] {
    def isDefinedAt(path: String) = delegate.hasPath(path)

    def apply(path: String): String = delegate.getString(path)

    def seqAs[W](path: String, default: ⇒ Iterable[W])(implicit convert: As[String, W]): IndexedSeq[W] =
      if (delegate.hasPath(path)) seqAs(path)(convert) else default.toVector

    def seqAs[W](path: String)(implicit convert: As[String, W]): IndexedSeq[W] =
      stringSeq(path) map wrappedConvert(convert.apply, path)

    def stringSeq(path: String, default: ⇒ Iterable[String]): IndexedSeq[String] =
      if (delegate.hasPath(path)) stringSeq(path) else default.toVector

    def stringSeq(path: String): IndexedSeq[String] =
      delegate.getStringList(path).asScala.toVector

    def durationOption(path: String): Option[Duration] = delegate.hasPath(path) option delegate.getDuration(path)
  }
}
