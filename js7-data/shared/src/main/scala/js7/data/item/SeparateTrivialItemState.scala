package js7.data.item

import monix.reactive.Observable

/** For orthogonality, for Items with a still(!) separate empty ItemState. */
trait SeparateTrivialItemState[A <: SeparateTrivialItemState[A]]
  extends InventoryItemState:
  this: A =>

  protected type Self <: InventoryItemState

  def path = item.path

  final def updateItem(item: companion.Item) =
    Right(item.toInitialItemState./*???*/asInstanceOf[companion.ItemState])

  override final def toSnapshotObservable =
    Observable.pure(item)

object SeparateTrivialItemState:
  trait Companion[A <: SeparateTrivialItemState[A]]
  extends InventoryItemState.Companion[A]
