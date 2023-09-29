package js7.data.item

import io.circe.{Codec, DecodingFailure, HCursor, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.item.InventoryItemKey.*
import scala.math.Ordered.orderingToOrdered

trait InventoryItemKey
{
  def companion: Companion[? <: InventoryItemKey]

  def path: InventoryItemPath

  def toTypedString: String

  def isAssignableToAgent: Boolean
}

object InventoryItemKey
{
  import InventoryItemPath.inventoryItemPathOrdering
  import VersionId.versionedIdOrdering
  
  implicit val inventoryItemKeyOrdering: Ordering[InventoryItemKey] =
    (a, b) => a.path.compare(b.path) match {
      case 0 => a match {
        case a: VersionedItemId_ => a.versionId.compare(b.asInstanceOf[VersionedItemId_].versionId)
        case _ => 0
      }
      case o => o
    }

  type Companion_ = Companion[? <: InventoryItemKey]

  trait Companion[A <: InventoryItemKey]
  {
    def checked(idString: String): Checked[A]

    def itemTypeName: String

    def pathTypeName: String
  }

  def jsonCodec(companions: Iterable[Companion_]): Codec[InventoryItemKey] = {
    val typeToCompanion = companions.toKeyedMap(_.pathTypeName)

    new Codec[InventoryItemKey]
    {
      def apply(key: InventoryItemKey) = Json.fromString(key.toTypedString)

      def apply(cursor: HCursor) =
        for
          string <- cursor.as[String]
          prefixAndId <- string indexOf ':' match {
            case i if i > 0 => Right((string take i, string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in InventoryItemKey: $string ", cursor.history))
          }
          prefix = prefixAndId._1
          idString = prefixAndId._2
          itemKey <- typeToCompanion.get(prefix)
            .toRight(DecodingFailure(s"Unrecognized type prefix in InventoryItemKey: $prefix ", cursor.history))
            .flatMap(_.checked(idString).toDecoderResult(cursor.history))
        yield itemKey
    }
  }

  trait AttachableToAgent {
    this: InventoryItemPath =>
  }
}
