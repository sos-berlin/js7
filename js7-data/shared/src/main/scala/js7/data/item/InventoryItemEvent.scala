package js7.data.item

import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.{ItemContainer, NoKeyEvent}

trait InventoryItemEvent extends NoKeyEvent:
  def key: InventoryItemKey


object InventoryItemEvent:
  def jsonCodec[S: ItemContainer.Companion]: TypedJsonCodec[InventoryItemEvent] =
    (BasicItemEvent.jsonCodec |
      UnsignedSimpleItemEvent.jsonCodec |
      SignedItemEvent.jsonCodec |
      UnsignedItemEvent.jsonCodec
    ).asInstanceOf[TypedJsonCodec[InventoryItemEvent]]
