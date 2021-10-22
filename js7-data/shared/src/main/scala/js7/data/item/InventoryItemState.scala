package js7.data.item

trait InventoryItemState
{
  type Item <: InventoryItem

  def item: Item
}
