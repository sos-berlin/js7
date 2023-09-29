package js7.data.item

trait UnsignedItemState extends InventoryItemState:
  protected type Self <: UnsignedItemState

  val companion: UnsignedItemState.Companion[Self]

  val item: companion.Item

object UnsignedItemState:
  trait Companion[A <: UnsignedItemState] extends InventoryItemState.Companion[A]:
    type Path <: UnsignedItemPath
    type Key <: UnsignedItemKey
    type Item <: UnsignedItem
