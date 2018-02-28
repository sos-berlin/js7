package com.sos.jobscheduler.data.filebased

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.filebased.TypedPath._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import java.nio.file.{Path, Paths}
import scala.collection.immutable.Iterable
import scala.reflect.ClassTag

trait TypedPath extends IsString {

  def companion: Companion[_ <: TypedPath]

  lazy val name: String = string.substring(string.lastIndexOf('/') + 1)

  /** Has to be called in every implementing constructor. */
  final def validate() = companion.check(string).force

  def nesting = string stripSuffix "/" count { _ == '/' }

  final def withTrailingSlash: String = if (string endsWith "/") string else s"$string/"

  final def withoutStartingSlash: String = string stripPrefix "/"

  /** The relative standard source file path. */
  def toFile(t: SourceType): Path =
    Paths.get(withoutStartingSlash + companion.sourceTypeToFilenameExtension(t))

  def asTyped[A <: TypedPath: TypedPath.Companion]: A = {
    val c = implicitly[TypedPath.Companion[A]]
    if (c == companion)
      this.asInstanceOf[A]
    else
      c.apply(string)
  }

  def checkedNameSyntax: Checked[this.type] =
    withoutStartingSlash.split('/').toVector traverse nameValidator.checked match {
      case Invalid(problem) ⇒ problem.head withKey toString
      case Valid(_) ⇒ Valid(this)
    }

  override def toString = toTypedString

  def pretty: String = s"${companion.camelName} $string"

  def toTypedString: String = s"${companion.camelName}:$string"
}

object TypedPath {
  val VersionSeparator = "@"
  private val nameValidator = new NameValidator(Set('-', '.'))

  implicit def ordering[P <: TypedPath]: Ordering[P] =
    (a, b) ⇒ a.string compare b.string

  type AnyCompanion = Companion[_ <: TypedPath]

  abstract class Companion[P <: TypedPath: ClassTag] extends IsString.Companion[P]
  {
    type Versioned = TypedPath.Versioned[P]
    final val Versioned = TypedPath.Versioned

    val name = getClass.simpleScalaName
    val NameOrdering: Ordering[P] = Ordering by { _.name }

    def apply(o: String): P

    def isEmptyAllowed = false
    def isSingleSlashAllowed = false

    final def checked(string: String): Checked[P] =
      check(string) map (_ ⇒ apply(string))

    private[TypedPath] def check(string: String): Checked[Unit] = {
      def errorString = s"$name '$string'"
      if (!isEmptyAllowed && string.isEmpty)
        Problem(s"Must not be the empty string in $errorString")
      else if (string.nonEmpty && !string.startsWith("/"))
        Problem(s"Absolute path expected in $errorString")
      else if (string.endsWith("/") && (!isSingleSlashAllowed || string != "/"))
        Problem(s"Trailing slash not allowed in $errorString")
      else if (string contains "//")
        Problem(s"Double slash not allowed in $errorString")
      else if (string.contains(","))
        Problem(s"Comma not allowed in $errorString")
      else
        Checked.unit
    }

    def sourceTypeToFilenameExtension: Map[SourceType, String]

    implicit val implicitCompanion: Companion[P] = this
    final val camelName: String = name stripSuffix "Path"

    final def typedPathClass: Class[P] = implicitClass[P]

    /** Converts a relative file path with normalized slahes (/) to a `TypedPath`. */
    def fromFile(normalized: String): Option[Checked[(P, SourceType)]] =
      sourceTypeToFilenameExtension.collectFirst { case (t, ext) if normalized endsWith ext ⇒
        Checked.catchNonFatal(apply("/" + normalized.dropRight(ext.length)) → t)
      }

    /**
     * Interprets a path as absolute.
     *
     * @param path A string starting with "./" is rejected
     */
    final def makeAbsolute(path: String): P =
      apply(absoluteString(path))

    override def toString = name
  }

  type Versioned_ = Versioned[_ <: TypedPath]

  final case class Versioned[P <: TypedPath](version: FileBasedVersion, path: P)
  //object Versioned {
  //  implicit def jsonEncoder[P <: TypedPath: Encoder]: ObjectEncoder[TypedPath.Versioned[P]] =
  //    o ⇒ JsonObject("path" → o.path.asJson, "version" → o.version.asJson)
  //
  //  implicit def jsonDecoder[P <: TypedPath: Decoder]: Decoder[Versioned[P]] =
  //    cursor ⇒
  //      for {
  //        path ← cursor.get[P]("path")
  //        version ← cursor.get[FileBasedVersion]("version")
  //      } yield Versioned(version, path)
  //}

  def fileToString(file: Path): String =
    file.toString.replaceChar(file.getFileSystem.getSeparator.charAt(0), '/')

  def jsonCodec(companions: Iterable[AnyCompanion]): CirceCodec[TypedPath] =
    new Encoder[TypedPath] with Decoder[TypedPath] {
      private val typeToCompanion = companions toKeyedMap (_.camelName)

      def apply(a: TypedPath) = Json.fromString(a.toTypedString)

      def apply(c: HCursor) =
        for {
          string ← c.as[String]
          prefixAndPath ← string indexOf ':' match {
            case i if i > 0 ⇒ Right((string take i, string.substring(i + 1)))
            case _ ⇒ Left(DecodingFailure(s"Missing type prefix in TypedPath: $string", Nil))
          }
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          typedPath ← typeToCompanion.get(prefix).map(_.apply(path))
            .toRight(DecodingFailure(s"Unrecognized type prefix in TypedPath: $prefix", Nil))
        } yield typedPath
    }

  /**
   * Interprets a path as absolute.
   *
   * @param path A string starting with "./" is rejected
   */
  private def absoluteString(path: String): String =
    if (isAbsolute(path))
      path
    else {
      require(!(path startsWith "./"), s"Relative path is not possible here: $path")
      s"/$path"
    }

  def isAbsolute(path: String): Boolean =
    path startsWith "/"
}
