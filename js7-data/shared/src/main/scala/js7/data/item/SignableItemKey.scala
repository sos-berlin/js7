package js7.data.item

import io.circe.Codec
import js7.data.item.InventoryItemKey.Companion

trait SignableItemKey extends InventoryItemKey
{
  def companion: Companion[_ <: SignableItemKey]
}

object SignableItemKey
{
  type Companion_ = Companion[_ <: SignableItemKey]

  trait Companion[A <: SignableItemKey] extends InventoryItemKey.Companion[A]

  def jsonCodec(companions: Iterable[SignableItemKey.Companion_]): Codec[SignableItemKey] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[SignableItemKey]]
}
