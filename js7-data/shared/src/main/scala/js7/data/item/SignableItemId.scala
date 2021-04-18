package js7.data.item

import io.circe.Codec
import js7.data.item.InventoryItemId.Companion

trait SignableItemId extends InventoryItemId
{
  def companion: Companion[_ <: SignableItemId]
}

object SignableItemId
{
  type Companion_ = Companion[_ <: SignableItemId]

  trait Companion[A <: SignableItemId] extends InventoryItemId.Companion[A]

  def jsonCodec(companions: Iterable[SignableItemId.Companion_]): Codec[SignableItemId] =
    InventoryItemId.jsonCodec(companions)
      .asInstanceOf[Codec[SignableItemId]]
}
