package js7.data.item

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.standards.Js7PathValidating
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.item.InventoryItemId._

trait InventoryItemId
{
  def companion: Companion[_ <: InventoryItemId]

  def toTypedString: String
}

object InventoryItemId
{
  type AnyCompanion = Companion[_ <: InventoryItemId]

  trait Companion[A <: InventoryItemId]
  {
    def checked(idString: String): Checked[A]

    def itemTypeName: String
  }

  def jsonCodec(companions: Iterable[AnyCompanion]): CirceCodec[InventoryItemId] = {
    val typeToCompanion = companions.toKeyedMap(_.itemTypeName)

    new Encoder[InventoryItemId] with Decoder[InventoryItemId]
    {
      def apply(id: InventoryItemId) = Json.fromString(id.toTypedString)

      def apply(cursor: HCursor) =
        for {
          string <- cursor.as[String]
          prefixAndPath <- string indexOf ':' match {
            case i if i > 0 => Right((string take i, string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in InventoryItemId: $string ", cursor.history))
          }
          prefix = prefixAndPath._1
          idString = prefixAndPath._2
          itemId <- typeToCompanion.get(prefix)
            .toRight(DecodingFailure(s"Unrecognized type prefix in InventoryItemId: $prefix ", cursor.history))
            .flatMap(_.checked(idString).toDecoderResult(cursor.history))
        } yield itemId
    }
  }
}
