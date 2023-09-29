package js7.data.item

trait UnsignedSimpleItemState
extends UnsignedItemState
with SimpleItemState:
  protected type Self <: UnsignedSimpleItemState
  val companion: UnsignedSimpleItemState.Companion[Self]

  val item: companion.Item
  def path: item.companion.Path

object UnsignedSimpleItemState:
  trait Companion[A <: UnsignedSimpleItemState]
  extends UnsignedItemState.Companion[A]
  with SimpleItemState.Companion[A]:
    type Key <: UnsignedSimpleItemPath
    type Path = Key
    type Item <: UnsignedSimpleItem
