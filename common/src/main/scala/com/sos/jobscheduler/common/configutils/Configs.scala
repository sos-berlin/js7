package com.sos.jobscheduler.common.configutils

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.convert.ConvertiblePartialFunctions.wrappedConvert
import com.sos.jobscheduler.base.convert.{As, ConvertiblePartialFunction}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.ClassLoaders.currentClassLoader
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
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
      logger.info(s"Reading configuration file $file")
      ConfigFactory.parseFile(file, Required)
    } else {
      logger.debug(s"No configuration file $file")
      ConfigFactory.empty
    }

  def loadResource(resource: JavaResource) = {
    logger.trace(s"Reading configuration JavaResource $resource")
    ConfigFactory.parseResourcesAnySyntax(resource.path, Required.setClassLoader(currentClassLoader))
  }

  implicit final class ConvertibleConfig(private val underlying: Config) extends ConvertiblePartialFunction[String, String]
  {
    def apply(path: String): String =
      underlying.getString(path)

    def isDefinedAt(path: String) =
      underlying.hasPath(path)
  }

  implicit final class RichConfig(private val underlying: Config) extends AnyVal
  {
    def seqAs[W](path: String, default: ⇒ Iterable[W])(implicit convert: As[String, W]): IndexedSeq[W] =
      if (underlying.hasPath(path)) seqAs(path)(convert) else default.toVector

    def seqAs[W](path: String)(implicit convert: As[String, W]): IndexedSeq[W] =
      stringSeq(path) map wrappedConvert(convert.apply, path)

    def stringSeq(path: String, default: ⇒ Iterable[String]): IndexedSeq[String] =
      if (underlying.hasPath(path)) stringSeq(path) else default.toVector

    def stringSeq(path: String): IndexedSeq[String] =
      underlying.getStringList(path).asScala.toVector

    def durationOption(path: String): Option[Duration] =
      underlying.hasPath(path) ? underlying.getDuration(path)

    def checkedPath[A](path: String)(f: String ⇒ Checked[A]): Checked[A] =
      if (!underlying.hasPath(path))
        Invalid(Problem(s"Missing configuration key '$path'"))
      else
        f(path)

    def ifPath[A](path: String)(f: String ⇒ A): Option[A] =
      underlying.hasPath(path) ? f(path)
  }
}
