package js7.data.item

trait UnsignedSimpleItemState extends InventoryItemState
{
  type Item <: UnsignedSimpleItem

  def item: Item
}
