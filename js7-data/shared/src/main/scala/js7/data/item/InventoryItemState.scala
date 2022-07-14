package js7.data.item

import js7.base.problem.Checked
import monix.reactive.Observable

trait InventoryItemState
{
  protected type Self <: InventoryItemState
  val companion: InventoryItemState.Companion[Self]

  val item: companion.Item

  def updateItem(item: companion.Item): Checked[companion.ItemState]

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(this)
}

object InventoryItemState
{
  type AnyCompanion = Companion[? <: InventoryItemState]

  trait Companion[A <: InventoryItemState] {
    type Key <: InventoryItemKey
    type Path <: InventoryItemPath
    type Item <: InventoryItem
    type ItemState = A
  }
}
