package js7.data.item

trait SimpleItemState extends InventoryItemState
{
  protected type Self <: SimpleItemState

  val companion: SimpleItemState.Companion[Self]

  val item: companion.Item
}

object SimpleItemState {
  trait Companion[A <: SimpleItemState] extends InventoryItemState.Companion[A]
  {
    type Path <: SimpleItemPath
  }
}
