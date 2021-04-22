package js7.data.item

import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.{JournaledState, NoKeyEvent}

trait InventoryItemEvent extends NoKeyEvent
{
  def id: InventoryItemKey
}

object InventoryItemEvent
{
  def jsonCodec[S <: JournaledState[S]](implicit S: JournaledState.Companion[S]) =
    (BasicItemEvent.jsonCodec |
      UnsignedSimpleItemEvent.jsonCodec |
      SignedItemEvent.jsonCodec
    ).asInstanceOf[TypedJsonCodec[InventoryItemEvent]]
}
