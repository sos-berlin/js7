package js7.data.item

trait SimpleItemState extends InventoryItemState
{
  protected type Item <: SimpleItem

  val item: Item
}

object SimpleItemState {
  trait Companion[A <: SimpleItemState] extends InventoryItemState.Companion[A]
  {
    type Path <: SimpleItemPath
  }
}
