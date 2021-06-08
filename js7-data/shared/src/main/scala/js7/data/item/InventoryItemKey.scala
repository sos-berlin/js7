package js7.data.item

import io.circe.{Codec, DecodingFailure, HCursor, Json}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.item.InventoryItemKey._

trait InventoryItemKey
{
  def companion: Companion[_ <: InventoryItemKey]

  def path: InventoryItemPath

  def toTypedString: String
}

object InventoryItemKey
{
  type Companion_ = Companion[_ <: InventoryItemKey]

  trait Companion[A <: InventoryItemKey]
  {
    def checked(idString: String): Checked[A]

    def itemTypeName: String
  }

  def jsonCodec(companions: Iterable[Companion_]): Codec[InventoryItemKey] = {
    val typeToCompanion = companions.toKeyedMap(_.itemTypeName)

    new Codec[InventoryItemKey]
    {
      def apply(key: InventoryItemKey) = Json.fromString(key.toTypedString)

      def apply(cursor: HCursor) =
        for {
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
        } yield itemKey
    }
  }
}
