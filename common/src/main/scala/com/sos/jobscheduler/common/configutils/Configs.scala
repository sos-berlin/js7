package js7.common.configutils

import js7.base.convert.ConvertiblePartialFunctions.wrappedConvert
import js7.base.convert.{As, ConvertiblePartialFunction}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalazStyle.OptionRichBoolean
import js7.common.scalautil.Logger
import js7.common.utils.JavaResource
import com.typesafe.config.ConfigRenderOptions.concise
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions, ConfigValue}
import java.nio.file.Files.exists
import java.nio.file.Path
import java.time.Duration
import scala.collection.immutable.VectorBuilder
import scala.jdk.CollectionConverters._
/**
  * @author Joacim Zschimmer
  */
object Configs
{
  private val InternalOriginDescription = "JobScheduler"
  private val SecretOriginDescription = "JobScheduler Secret"
  private val Required = ConfigParseOptions.defaults.setAllowMissing(false)
  private val logger = Logger(getClass)

  def parseConfigIfExists(file: Path, secret: Boolean): Config =
    if (exists(file)) {
      logger.info(s"Reading configuration file $file")
      var options = Required
      if (secret) options = options.setOriginDescription(SecretOriginDescription)
      ConfigFactory.parseFile(file.toFile, options)
    } else {
      logger.debug(s"No configuration file $file")
      ConfigFactory.empty
    }

  def loadResource(resource: JavaResource, internal: Boolean = false) = {
    logger.trace(s"Reading configuration JavaResource $resource")
    var options = Required.setClassLoader(resource.classLoader)
    if (internal) options = options.setOriginDescription(InternalOriginDescription)
    ConfigFactory.parseResourcesAnySyntax(resource.path, options)
  }

  def logConfig(config: Config): Unit =
    for (line <- renderConfig(config)) logger.debug(line)

  def renderConfig(config: Config): Seq[String] = {
    val builder = new VectorBuilder[String]
    val sb = new StringBuilder
    for (entry <- config.entrySet.asScala.view
      .filter(_.getValue.origin.description.replaceFirst(": [0-9]+(-[0-9]+)?", "") != InternalOriginDescription)
      .toVector
      .sortBy(_.getKey))
    {
      sb.clear()
      sb ++= renderKeyValue(entry.getKey, entry.getValue)
      if (!entry.getValue.origin.description.startsWith(SecretOriginDescription)) {
        sb ++= " ("
        sb ++= entry.getValue.origin.description
        sb += ')'
      }
      builder += sb.toString
    }
    builder.result()
  }

  private def renderKeyValue(key: String, value: ConfigValue): String = {
    val v =
      if (key.endsWith("password") || value.origin.description.startsWith(SecretOriginDescription))
        "(secret)"
      else
        value.render(concise.setJson(false))
      s"$key=$v"
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
    def seqAs[W](path: String, default: => Iterable[W])(implicit convert: As[String, W]): IndexedSeq[W] =
      if (underlying.hasPath(path)) seqAs(path)(convert) else default.toVector

    def seqAs[W](path: String)(implicit convert: As[String, W]): IndexedSeq[W] =
      stringSeq(path) map wrappedConvert(convert.apply, path)

    def stringSeq(path: String, default: => Iterable[String]): IndexedSeq[String] =
      if (underlying.hasPath(path)) stringSeq(path) else default.toVector

    def stringSeq(path: String): IndexedSeq[String] =
      underlying.getStringList(path).asScala.toVector

    def durationOption(path: String): Option[Duration] =
      underlying.hasPath(path) ? underlying.getDuration(path)

    def checkedPath[A](path: String)(f: String => Checked[A]): Checked[A] =
      if (!underlying.hasPath(path))
        Left(Problem(s"Missing configuration key '$path'"))
      else
        f(path)

    def ifPath[A](path: String)(f: String => A): Option[A] =
      underlying.hasPath(path) ? f(path)
  }
}
