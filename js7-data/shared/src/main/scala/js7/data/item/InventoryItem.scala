package js7.data.item

import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
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

    val Id: InventoryItemId.Companion[Id]

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

  def jsonCodec(companions: Seq[Companion]): TypedJsonCodec[InventoryItem] =
    TypedJsonCodec(companions.map(_.subtype): _*)
}
