package js7.data.item

import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.InventoryItem.Companion
import scala.reflect.ClassTag

trait InventoryItem
{
  protected type Self <: InventoryItem

  val companion: Companion[Self]

  def id: InventoryItemKey
  def itemRevision: Option[ItemRevision]

  // Accelerate usage in Set[InventoryItem], for example in AgentDriver's CommandQueue
  override def hashCode = 31 * id.hashCode + itemRevision.hashCode
}

object InventoryItem
{
  type Companion_ = Companion[_ <: InventoryItem]

  trait Companion[A <: InventoryItem]
  {
    type Item <: A
    type Key <: InventoryItemKey

    val Key: InventoryItemKey.Companion[Key]

    def cls: Class[A]

    implicit def jsonCodec: Codec.AsObject[A]

    val typeName = getClass.simpleScalaName

    def subtype: Subtype[A] =
      Subtype(jsonCodec)(ClassTag(cls))

    def jsonEncoder: Encoder.AsObject[A] =
      jsonCodec

    def jsonDecoder: Decoder[A] =
      jsonCodec

    override def toString = typeName
  }

  def jsonCodec(companions: Seq[Companion_]): TypedJsonCodec[InventoryItem] =
    TypedJsonCodec(companions.map(_.subtype): _*)
}
