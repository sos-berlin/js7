package js7.data.item

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.item.VersionedControlPath.*
import scala.reflect.ClassTag

trait VersionedControlPath extends UnsignedItemPath:
  def companion: Companion[? <: VersionedControlPath]

  final lazy val name: String =
    string.substring(string.lastIndexOf('/') + 1)

  final def requireNonAnonymous(): Unit =
    companion.checked(string).orThrow

object VersionedControlPath:
  implicit def ordering[P <: VersionedControlPath]: Ordering[P] =
    (a, b) => a.string compare b.string match
      case 0 => a.companion.name compare b.companion.name
      case o => o

  type AnyCompanion = Companion[? <: VersionedControlPath]

  implicit final class ImplicitItemPath[P <: VersionedControlPath](private val underlying: P) extends AnyVal:
    def ~(version: String): UnsignedVersionedItemId[P] =
      this ~ VersionId(version)

    def ~(v: VersionId): UnsignedVersionedItemId[P] =
      UnsignedVersionedItemId(underlying, v)

  abstract class Companion[P <: VersionedControlPath: ClassTag]
  extends UnsignedItemPath.Companion[P]:
    final val NameOrdering: Ordering[P] = Ordering.by(_.name)
    implicit override final val self: Companion[P] = this

    private def P = this

    object VersionedControlIdCompanion extends UnsignedVersionedItemId.Companion[P]:
      def apply(idString: String): UnsignedVersionedItemId[P] =
        checked(idString).orThrow

      def apply(path: P, versionId: VersionId) =
        path ~ versionId

      val pathCompanion = P

      // A versioned pathTypeName may not differ from its itemTypePath
      def pathTypeName = itemTypeName

    override def toString = name

    implicit override final val jsonEncoder: Encoder[P] =
      o => Json.fromString(o.string)

    implicit override final val jsonDecoder: Decoder[P] =
      c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))

  // TODO Same as VersionedItemPath.jsonCodec
  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[VersionedControlPath] =
    new Codec[VersionedControlPath]:
      private val typeToCompanion = companions.toKeyedMap(_.itemTypeName)

      def apply(a: VersionedControlPath) = Json.fromString(a.toTypedString)

      def apply(c: HCursor) =
        for
          string <- c.as[String]
          prefixAndPath <- string indexOf ':' match
            case i if i > 0 => Right((string take i, string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in VersionedControlPath: $string", c.history))
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          itemPath <- typeToCompanion.get(prefix)
            .toRight(Problem.pure(s"Unrecognized type prefix in VersionedControlPath: $prefix"))
            .flatMap(_.checked(path))
            .toDecoderResult(c.history)
        yield itemPath.asInstanceOf[VersionedControlPath]/*???*/
