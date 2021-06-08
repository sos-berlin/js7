package js7.data.item

import js7.data.event.Event

trait ItemAddedOrChanged
{
  this: Event =>

  def item: InventoryItem
}
