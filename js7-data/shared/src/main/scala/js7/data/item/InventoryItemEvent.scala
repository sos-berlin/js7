package js7.data.item

import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.NoKeyEvent

trait InventoryItemEvent extends NoKeyEvent
{
  // Or type Key = InventoryItemId ???
  def id: InventoryItemId
}

object InventoryItemEvent
{
  def jsonCodec(companions: Seq[InventoryItem.Companion]): TypedJsonCodec[InventoryItemEvent] = {
    implicit val itemJsonCodec = InventoryItem.jsonCodec(companions)
    implicit val idJsonCodec = InventoryItemId.jsonCodec(companions.map(_.idCompanion))

    val simpleItemCompanions = companions.collect { case o: SimpleItem.Companion => o  }
    val jsonCodec =
      CommonItemEvent.jsonCodec(companions) |
      SimpleItemEvent.jsonCodec(simpleItemCompanions)
    jsonCodec.asInstanceOf[TypedJsonCodec[InventoryItemEvent]]
  }
}
