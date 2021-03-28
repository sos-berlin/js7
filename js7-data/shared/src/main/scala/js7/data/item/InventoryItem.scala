package js7.data.item

import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.typed.Subtype
import js7.base.utils.ScalaUtils.syntax._
import scala.reflect.ClassTag

trait InventoryItem

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
}
