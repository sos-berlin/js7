package js7.data.item

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import java.nio.file.{Path, Paths}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.VersionedItemPath._
import scala.reflect.ClassTag

trait VersionedItemPath extends InventoryItemPath
{
  def companion: Companion[_ <: VersionedItemPath]

  final lazy val name: String = string.substring(string.lastIndexOf('/') + 1)

  final def requireNonAnonymous(): Unit =
    companion.checked(string).orThrow

  final def withTrailingSlash = s"$string/"

  /** The relative standard source file path. */
  def toFile(t: SourceType): Path =
    Paths.get(string + companion.sourceTypeToFilenameExtension(t))

  final def asTyped[P <: VersionedItemPath](implicit P: VersionedItemPath.Companion[P]): P =
    if (P == companion)
      this.asInstanceOf[P]
    else
      P.apply(string)

  final def cast[P <: VersionedItemPath](implicit P: VersionedItemPath.Companion[P]): P = {
    if (P != companion) throw new ClassCastException(s"Expected ${companion.name}, but is: $toString")
    this.asInstanceOf[P]
  }

  final def isAnonymous = this == companion.Anonymous

  override def toString = toTypedString

  final def toTypedString: String = s"${companion.itemTypeName}:$string"
}

object VersionedItemPath
{
  implicit def ordering[P <: VersionedItemPath]: Ordering[P] =
    (a, b) => a.string compare b.string match {
      case 0 => a.companion.name compare b.companion.name
      case o => o
    }

  type AnyCompanion = Companion[_ <: VersionedItemPath]

  implicit final class ImplicitItemPath[P <: VersionedItemPath](private val underlying: P) extends AnyVal {
    def ~(version: String)(implicit P: VersionedItemPath.Companion[P]): VersionedItemId[P] =
      this ~ VersionId(version)

    def ~(v: VersionId): VersionedItemId[P] =
      VersionedItemId(underlying, v)
  }

  abstract class Companion[P <: VersionedItemPath: ClassTag]
  extends InventoryItemPath.Companion[P]
  {
    final val NameOrdering: Ordering[P] = Ordering.by(_.name)
    final lazy val Anonymous: P = unchecked("⊥")
    final lazy val NoId: VersionedItemId[P] = Anonymous ~ VersionId.Anonymous
    implicit override final val self: Companion[P] = this

    private def P = this

    override def checked(string: String): Checked[P] =
      if (string == Anonymous.string)
        Left(Problem(s"Anonymous $name not allowed here"))
      else
        super.checked(string)

    def sourceTypeToFilenameExtension: Map[SourceType, String]

    object VersionedItemIdCompanion extends VersionedItemId.Companion[P] {
      def apply(idString: String): VersionedItemId[P] =
        checked(idString).orThrow

      def apply(path: P, versionId: VersionId) =
        path ~ versionId

      val pathCompanion = P
    }

    final val itemTypeName: String = name stripSuffix "Path"
    final val itemPathClass: Class[P] = implicitClass[P]

    /** Converts a relative file path with normalized slahes (/) to a `VersionedItemPath`. */
    final def fromFile(normalized: String): Option[Checked[(P, SourceType)]] =
      sourceTypeToFilenameExtension.collectFirst { case (t, ext) if normalized endsWith ext =>
        checked(normalized.dropRight(ext.length)).map(_ -> t)
      }

    override def toString = name

    implicit override final val jsonEncoder: Encoder[P] = o => {
      if (o == Anonymous) throw new IllegalArgumentException(s"JSON serialize $name.Anonymous?")
      Json.fromString(o.string)
    }

    implicit override final val jsonDecoder: Decoder[P] =
      c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))
  }

  def fileToString(file: Path): String =
    file.toString.replaceChar(file.getFileSystem.getSeparator.charAt(0), '/')

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[VersionedItemPath] =
    new Codec[VersionedItemPath] {
      private val typeToCompanion = companions.toKeyedMap(_.itemTypeName)

      def apply(a: VersionedItemPath) = Json.fromString(a.toTypedString)

      def apply(c: HCursor) =
        for {
          string <- c.as[String]
          prefixAndPath <- string indexOf ':' match {
            case i if i > 0 => Right((string take i, string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in VersionedItemPath: $string", c.history))
          }
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          itemPath <- typeToCompanion.get(prefix).map(_.apply(path))
            .toRight(DecodingFailure(s"Unrecognized type prefix in VersionedItemPath: $prefix", c.history))
        } yield itemPath
    }
}
