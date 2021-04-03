package js7.data.item

import io.circe.Decoder.Result
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, HCursor}
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.Subtype
import js7.base.circeutils.typed.TypedJsonCodec.TypeFieldName
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.InventoryItem.Companion
import scala.reflect.ClassTag

trait InventoryItem
{
  val companion: Companion
  def id: InventoryItemId
  def itemRevision: Option[ItemRevision]
}

object InventoryItem
{
  trait Companion
  {
    type Item <: InventoryItem
    type Id <: InventoryItemId

    val idCompanion: InventoryItemId.Companion[Id]

    def cls: Class[Item]

    implicit def jsonCodec: Codec.AsObject[Item]

    val typeName = getClass.simpleScalaName

    final def subtype: Subtype[Item] =
      Subtype(jsonCodec)(ClassTag(cls))

    def jsonEncoder: Encoder.AsObject[Item] =
      jsonCodec

    def jsonDecoder: Decoder[Item] =
      jsonCodec

    override def toString = typeName
  }

  def jsonCodec(companions: Seq[Companion]): Codec.AsObject[InventoryItem] = {
    val typeToCompanion = companions.toKeyedMap(_.typeName)
    new Codec.AsObject[InventoryItem] {
      def encodeObject(item: InventoryItem) =
        (TypeFieldName -> item.companion.typeName.asJson) +:
          item.companion.jsonEncoder.encodeObject(item.asInstanceOf[item.companion.Item])

      def apply(cursor: HCursor): Result[InventoryItem] =
        for {
          typeName <- cursor.get[String](TypeFieldName)
          companion <- typeToCompanion.checked(typeName).toDecoderResult(cursor.history)
          item <- companion.jsonDecoder.tryDecode(cursor)
        } yield item
    }
  }
}
