package js7.data.item

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.item.VersionedItemPath.*
import scala.reflect.ClassTag

trait VersionedItemPath extends InventoryItemPath
{
  def companion: Companion[? <: VersionedItemPath]

  final lazy val name: String =
    string.substring(string.lastIndexOf('/') + 1)

  final def requireNonAnonymous(): Unit =
    companion.checked(string).orThrow

  final def withTrailingSlash = s"$string/"

  final def asTyped[P <: VersionedItemPath](implicit P: VersionedItemPath.Companion[P]): P =
    if (P == companion)
      this.asInstanceOf[P]
    else
      P.apply(string)

  final def isAnonymous =
    this == companion.Anonymous
}

object VersionedItemPath
{
  implicit def ordering[P <: VersionedItemPath]: Ordering[P] =
    (a, b) => a.string compare b.string match {
      case 0 => a.companion.name compare b.companion.name
      case o => o
    }

  type AnyCompanion = Companion[? <: VersionedItemPath]

  implicit final class ImplicitItemPath[P <: VersionedItemPath](private val underlying: P) extends AnyVal {
    def ~(version: String): VersionedItemId[P] =
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

    object VersionedItemIdCompanion extends VersionedItemId.Companion[P] {
      def apply(idString: String): VersionedItemId[P] =
        checked(idString).orThrow

      def apply(path: P, versionId: VersionId) =
        path ~ versionId

      val pathCompanion = P

      // A versioned pathTypeName may not differ from its itemTypePath
      def pathTypeName = itemTypeName
    }

    override def toString = name

    implicit override final val jsonEncoder: Encoder[P] = o => {
      if (o == Anonymous) throw new IllegalArgumentException(s"JSON serialize $name.Anonymous?")
      Json.fromString(o.string)
    }

    implicit override final val jsonDecoder: Decoder[P] =
      c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))
  }

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
          itemPath <- typeToCompanion.get(prefix)
            .toRight(Problem.pure(s"Unrecognized type prefix in VersionedItemPath: $prefix"))
            .flatMap(_.checked(path))
            .toDecoderResult(c.history)
        } yield itemPath.asInstanceOf[VersionedItemPath]/*???*/
    }
}
