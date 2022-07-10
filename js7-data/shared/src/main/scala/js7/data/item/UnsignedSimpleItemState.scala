package js7.data.item

import js7.base.problem.Checked

trait UnsignedSimpleItemState extends SimpleItemState
{
  protected type Self <: UnsignedSimpleItemState
  val companion: UnsignedSimpleItemState.Companion[Self]

  val item: companion.Item
  def path: item.companion.Path

  def updateItem(item: companion.Item): Checked[companion.ItemState]
}

object UnsignedSimpleItemState
{
  trait Companion[A <: UnsignedSimpleItemState] extends SimpleItemState.Companion[A]
  {
    type Path <: UnsignedSimpleItemPath
    type ItemState <: UnsignedSimpleItemState
    type Item <: UnsignedSimpleItem
  }
}
