package js7.data.item

import io.circe.Codec
import js7.data.item.InventoryItemId.Companion

trait UnsignedSimpleItemId extends SimpleItemId
{
  def companion: Companion[_ <: UnsignedSimpleItemId]
}

object UnsignedSimpleItemId
{
  type Companion_ = Companion[_ <: UnsignedSimpleItemId]

  trait Companion[A <: UnsignedSimpleItemId] extends SimpleItemId.Companion[A]

  def jsonCodec(companions: Iterable[UnsignedSimpleItemId.Companion_]): Codec[UnsignedSimpleItemId] =
    InventoryItemId.jsonCodec(companions)
      .asInstanceOf[Codec[UnsignedSimpleItemId]]
}
