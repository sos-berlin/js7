package js7.base.configutils

import cats.Monoid
import com.typesafe.config.ConfigRenderOptions.concise
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigParseOptions, ConfigValue}
import io.circe.Json
import java.io.File
import java.nio.file.Files.exists
import java.nio.file.Path
import java.time.Duration
import js7.base.circeutils.AnyJsonCodecs
import js7.base.convert.ConvertiblePartialFunctions.wrappedConvert
import js7.base.convert.{As, ConvertiblePartialFunction}
import js7.base.generic.GenericString
import js7.base.io.JavaResource
import js7.base.log.Logger
import js7.base.problem.Checked.catchExpected
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StringInterpolators.interpolate
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object Configs:

  private val InternalOriginDescription = "JS7"
  private val SecretOriginDescription = "JS7 Secret"
  private val Required = ConfigParseOptions.defaults.setAllowMissing(false)
  private lazy val logger = Logger[this.type]

  def configIf(predicate: Boolean, config: => Config): Config =
    if predicate then config
    else ConfigFactory.empty

  def parseConfigIfExists(file: Path, secret: Boolean): Config =
    if exists(file) then
      logger.info(s"Reading configuration file $file")
      var options = Required
      if secret then options = options.setOriginDescription(SecretOriginDescription)
      ConfigFactory.parseFile(file.toFile, options)
    else
      logger.debug(s"No configuration file $file")
      ConfigFactory.empty

  def loadResource(resource: JavaResource) =
    logger.trace(s"Reading configuration JavaResource $resource")
    var options = Required.setClassLoader(resource.classLoader)
    options = options.setOriginDescription(InternalOriginDescription)
    ConfigFactory.parseResourcesAnySyntax(resource.path, options)

  def logConfig(config: Config): Unit =
    for entry <- config.entrySet.asScala.view.toVector.sortBy(_.getKey) do
      def line = renderValue(entry.getKey, entry.getValue)
      if isInternal(entry.getValue) then
        logger.trace(line)
      else
        logger.debug(line)

  private[configutils] def renderValue(key: String, value: ConfigValue): String =
    val sb = new StringBuilder(128)
    sb ++= key
    sb += '='
    val secret = isSecret(value)
    sb ++=
      (if secret || key.endsWith("password") then
        "(secret)"
      else
        value.render(concise))
    if !secret && !isInternal(value) then
      sb ++= " ("
      sb ++= value.origin.description
      sb += ')'
    sb.toString

  private val descriptionRegEx = ": [0-9]+(-[0-9]+)?".r

  private def isInternal(value: ConfigValue) =
    descriptionRegEx.replaceFirstIn(value.origin.description, "") == InternalOriginDescription

  private def isSecret(value: ConfigValue) =
    value.origin.description.startsWith(SecretOriginDescription)

  implicit final class ConvertibleConfig(private val underlying: Config) extends ConvertiblePartialFunction[String, String]:
    def apply(path: String): String =
      underlying.getString(path)

    def isDefinedAt(path: String) =
      underlying.hasPath(path)

  implicit final class RichConfig(private val underlying: Config) extends AnyVal:
    def getBoolean(path: String, default: Boolean): Boolean =
      if underlying.hasPath(path) then
        underlying.getBoolean(path)
      else
        default

    def seqAs[W](path: String, default: => Iterable[W])(implicit convert: As[String, W]): IndexedSeq[W] =
      if underlying.hasPath(path) then seqAs(path)(convert) else default.toVector

    def seqAs[W](path: String)(implicit convert: As[String, W]): IndexedSeq[W] =
      stringSeq(path) map wrappedConvert(convert.apply, path)

    def stringSeq(path: String, default: => Iterable[String]): IndexedSeq[String] =
      if underlying.hasPath(path) then stringSeq(path) else default.toVector

    def stringSeq(path: String): IndexedSeq[String] =
      underlying.getStringList(path).asScala.toVector

    def durationOption(path: String): Option[Duration] =
      underlying.hasPath(path) ? underlying.getDuration(path)

    def checkedPath[A](path: String)(f: String => Checked[A]): Checked[A] =
      if !underlying.hasPath(path) then
        Left(Problem(s"Missing configuration key '$path'"))
      else
        f(path)

    def ifPath[A](path: String)(f: String => A): Option[A] =
      underlying.hasPath(path) ? f(path)

    def finiteDuration(path: String): Checked[FiniteDuration] =
      catchExpected[ConfigException](
        underlying.getDuration(path).toFiniteDuration)

    def memorySizeAsInt(path: String): Checked[Int] =
      val bigInteger = underlying.getMemorySize(path).toBytesBigInteger
      if bigInteger.bitLength >= 32 then
        Left(Problem(s"Number is to big: $path = $bigInteger"))
      else
        Right(bigInteger.intValue)

  implicit val configMonoid: Monoid[Config] =
    new Monoid[Config]:
      val empty = ConfigFactory.empty
      def combine(a: Config, b: Config) = b withFallback a

  implicit final class HoconStringInterpolator(private val sc: StringContext) extends AnyVal:
    def config(args: Any*)(implicit enclosing: sourcecode.Enclosing): Config =
      val configString = interpolate(sc, args, toHoconString)
      try ConfigFactory.parseString(
        configString,
        ConfigParseOptions.defaults().setOriginDescription(enclosing.value))
      catch { case NonFatal(t) =>
        logger.warn(s"${enclosing.value} => ${t.toStringWithCauses}")
        throw t
      }

    def configString(args: Any*): String =
      interpolate(sc, args, toHoconString)

  private def toHoconString(value: Any): String =
    value match
      case v: ToHoconString => v.toHoconString

      case v: FiniteDuration => v.toHoconString

      case v @ (_: String | _: GenericString | _: Path | _: File) =>
        val str = v match
          case o: GenericString => o.string
          case o => o.toString
        val json = Json.fromString(str).toString
        // Strip quotes. Interpolation is expected to occur already in quotes: "$var"
        json.substring(1, json.length - 1)

      case v: Map[?, ?] =>
        v.asInstanceOf[Map[Any, Any]].view
          .map:
            case (k, v) => toHoconString(k) + ": " + toHoconString(v)
          .mkString("{ ", ", ", " }")

      case v: Iterable[?] =>
        v.view.map(toHoconString).mkString("[ ", ", ", " ]")

      case v: Array[?] =>
        v.view.map(toHoconString).mkString("[ ", ", ", " ]")

      case v: java.util.Map[?, ?] =>
        toHoconString(v.asScala)

      case v: java.lang.Iterable[?] =>
        toHoconString(v.asScala)

      case v => AnyJsonCodecs.anyToJson(v).toString
