package js7.data.item

import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.NoKeyEvent

trait InventoryItemEvent extends NoKeyEvent
{
  def id: InventoryItemId
}

object InventoryItemEvent
{
  def jsonCodec(companions: Seq[InventoryItem.Companion_]): TypedJsonCodec[InventoryItemEvent] = {
    implicit val itemJsonCodec = InventoryItem.jsonCodec(companions)
    implicit val idJsonCodec = InventoryItemId.jsonCodec(companions.map(_.Id))

    val simpleItemCompanions = companions.collect { case o: SimpleItem.Companion_ => o  }
    val jsonCodec =
      CommonItemEvent.jsonCodec(companions) |
      SimpleItemEvent.jsonCodec(simpleItemCompanions)
    jsonCodec.asInstanceOf[TypedJsonCodec[InventoryItemEvent]]
  }
}
