package js7.data.item

trait InventoryItemState
{
  protected type Item <: InventoryItem

  def item: Item
}
