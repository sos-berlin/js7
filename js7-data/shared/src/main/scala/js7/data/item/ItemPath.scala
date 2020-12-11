package js7.data.item

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}
import java.nio.file.{Path, Paths}
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.generic.GenericString
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked.Ops
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.problem.{Checked, Problem}
import js7.base.standards.NameValidator
import js7.base.utils.Collections.implicits.RichTraversable
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.ItemPath._
import js7.data.item.VersionedItemId.VersionSeparator
import scala.reflect.ClassTag

trait ItemPath extends GenericString
{
  def companion: Companion[_ <: ItemPath]

  final lazy val name: String = string.substring(string.lastIndexOf('/') + 1)

  final def requireNonAnonymous(): Unit =
    companion.checked(string).orThrow

  final def nesting = string.stripSuffix("/").count(_ == '/')

  final def withTrailingSlash: String = if (string endsWith "/") string else s"$string/"

  final def withoutStartingSlash: String = string stripPrefix "/"

  /** The relative standard source file path. */
  def toFile(t: SourceType): Path =
    Paths.get(withoutStartingSlash + companion.sourceTypeToFilenameExtension(t))

  final def asTyped[P <: ItemPath](implicit P: ItemPath.Companion[P]): P = {
    if (P == companion)
      this.asInstanceOf[P]
    else
      P.apply(string)
  }

  final def cast[P <: ItemPath](implicit P: ItemPath.Companion[P]): P = {
    if (P != companion) throw new ClassCastException(s"Expected ${companion.name}, but is: $toString")
    this.asInstanceOf[P]
  }

  final def officialSyntaxChecked: Checked[this.type] =
    if (string startsWith InternalPrefix)
      Problem(s"Internal path is not allowed here: $this")
    else
      withoutStartingSlash.split('/')
        .toVector.traverse(officialSyntaxNameValidator.checked(typeName = companion.name, _))
      match {
        case Left(problem) => problem.head
        case Right(_) => Right(this)
      }

  final def isAnonymous = this == companion.Anonymous

  final def isGenerated = string startsWith InternalPrefix

  override def toString = toTypedString

  final def pretty: String = s"${companion.camelName} $string"

  final def toTypedString: String = s"${companion.camelName}:$string"
}

object ItemPath
{
  val InternalPrefix = "/?/"
  private val ForbiddenCharacters = Set[Char](VersionSeparator(0), ','/*reserved*/)
  private val officialSyntaxNameValidator = new NameValidator(Set('-', '.'))

  implicit def ordering[P <: ItemPath]: Ordering[P] =
    (a, b) => a.string compare b.string match {
      case 0 => a.companion.name compare b.companion.name
      case o => o
    }

  type AnyCompanion = Companion[_ <: ItemPath]

  implicit final class ImplicitItemPath[P <: ItemPath](private val underlying: P) extends AnyVal {
    def ~(version: String): VersionedItemId[P] = this ~ VersionId(version)
    def ~(v: VersionId): VersionedItemId[P] = VersionedItemId(underlying, v)
  }

  abstract class Companion[P <: ItemPath: ClassTag] extends GenericString.Checked_[P]
  {
    final val NameOrdering: Ordering[P] = Ordering.by(_.name)
    final lazy val Anonymous: P = unchecked(InternalPrefix + "anonymous")
    final lazy val NoId: VersionedItemId[P] = Anonymous ~ VersionId.Anonymous

    def isEmptyAllowed = false
    def isSingleSlashAllowed = false

    /** Must be non-anonymous, too. */
    override final def checked(string: String): Checked[P] =
      if (string == Anonymous.string)
        Problem(s"An anonymous $name is not allowed")
      else
        check(string).flatMap(_ => super.checked(string))

    final private[ItemPath] def check(string: String): Checked[Unit] =
      if (!isEmptyAllowed && string.isEmpty)
        EmptyStringProblem(name)
      else if (string.nonEmpty && !string.startsWith("/"))
        Problem(s"$name must be an absolute path, not: $string")
      else if (string.endsWith("/") && (!isSingleSlashAllowed || string != "/"))
        Problem(s"$name must not end with a slash: $string")
      else if (string.contains("//") || string.exists(ForbiddenCharacters))
        InvalidNameProblem(name, string)
      else
        Checked.unit

    def sourceTypeToFilenameExtension: Map[SourceType, String]

    final implicit val implicitCompanion: Companion[P] = this
    final val camelName: String = name stripSuffix "Path"

    final def itemPathClass: Class[P] = implicitClass[P]

    /** Converts a relative file path with normalized slahes (/) to a `ItemPath`. */
    final def fromFile(normalized: String): Option[Checked[(P, SourceType)]] =
      sourceTypeToFilenameExtension.collectFirst { case (t, ext) if normalized endsWith ext =>
        checked("/" + normalized.dropRight(ext.length)).map(_ -> t)
      }

    /**
     * Interprets a path as absolute.
     *
     * @param path A string starting with "./" is rejected
     */
    final def makeAbsolute(path: String): P =
      apply(absoluteString(path))

    override def toString = name

    override final implicit val jsonEncoder: Encoder[P] = o => {
      if (o == Anonymous) throw new IllegalArgumentException(s"JSON serialize $name.Anonymous?")
      Json.fromString(o.string)
    }

    override final implicit val jsonDecoder: Decoder[P] =
      c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))
  }

  def fileToString(file: Path): String =
    file.toString.replaceChar(file.getFileSystem.getSeparator.charAt(0), '/')

  def jsonCodec(companions: Iterable[AnyCompanion]): CirceCodec[ItemPath] =
    new Encoder[ItemPath] with Decoder[ItemPath] {
      private val typeToCompanion = companions toKeyedMap (_.camelName)

      def apply(a: ItemPath) = Json.fromString(a.toTypedString)

      def apply(c: HCursor) =
        for {
          string <- c.as[String]
          prefixAndPath <- string indexOf ':' match {
            case i if i > 0 => Right((string take i, string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in ItemPath: $string", c.history))
          }
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          itemPath <- typeToCompanion.get(prefix).map(_.apply(path))
            .toRight(DecodingFailure(s"Unrecognized type prefix in ItemPath: $prefix", c.history))
        } yield itemPath
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

  implicit val jsonEncoder: Encoder.AsObject[ItemPath] = o => JsonObject(
    "TYPE" -> Json.fromString(o.companion.name),
    "path" -> Json.fromString(o.string))

  def jsonDecoder(toItemPathCompanion: String => Checked[ItemPath.AnyCompanion]): Decoder[ItemPath] =
    c => for {
      typ <- c.get[String]("TYPE")
      path <- c.get[String]("path")
      t <- toItemPathCompanion(typ).toDecoderResult(c.history)
      itemPath <- t.checked(path).toDecoderResult(c.history)
    } yield itemPath
}
