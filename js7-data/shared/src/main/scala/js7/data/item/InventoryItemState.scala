package js7.data.item

import monix.reactive.Observable

trait InventoryItemState
{
  protected type Self <: InventoryItemState
  val companion: InventoryItemState.Companion[Self]

  protected type Item <: InventoryItem
  val item: Item

  final def path: item.companion.Path =
    item.path

  def toSnapshotObservable: Observable[Any] =
    Observable.pure(this)
}

object InventoryItemState
{
  type AnyCompanion = Companion[_ <: InventoryItemState]

  trait Companion[A <: InventoryItemState] {
    type Path <: InventoryItemPath
  }
}
