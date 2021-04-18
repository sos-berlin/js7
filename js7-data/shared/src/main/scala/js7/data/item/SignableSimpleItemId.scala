package js7.data.item

import io.circe.Codec
import js7.data.item.InventoryItemId.Companion

trait SignableSimpleItemId extends SimpleItemId with SignableItemId
{
  def companion: Companion[_ <: SignableSimpleItemId]
}

object SignableSimpleItemId
{
  type Companion_ = Companion[_ <: SignableSimpleItemId]

  trait Companion[A <: SignableSimpleItemId] extends SimpleItemId.Companion[A] with SignableItemId.Companion[A]

  def jsonCodec(companions: Iterable[SignableSimpleItemId.Companion_]): Codec[SignableSimpleItemId] =
    InventoryItemId.jsonCodec(companions)
      .asInstanceOf[Codec[SignableSimpleItemId]]
}
