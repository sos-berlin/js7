package js7.data.item

import io.circe.Codec
import js7.data.item.InventoryItemKey.Companion

trait UnsignedItemKey extends InventoryItemKey
{
  def companion: Companion[_ <: UnsignedItemKey]
}

object UnsignedItemKey
{
  type Companion_ = Companion[_ <: UnsignedItemKey]

  trait Companion[A <: UnsignedItemKey] extends InventoryItemKey.Companion[A]

  def jsonCodec(companions: Iterable[UnsignedItemKey.Companion_]): Codec[UnsignedItemKey] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[UnsignedItemKey]]
}
