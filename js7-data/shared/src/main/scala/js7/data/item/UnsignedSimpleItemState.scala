package js7.data.item

trait UnsignedSimpleItemState extends InventoryItemState
{
  protected type Item <: UnsignedSimpleItem

  def item: Item
}
