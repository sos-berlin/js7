package js7.data.item

import monix.reactive.Observable

/** For orthogonality, for Items which are the (empty) ItemState. */
trait TrivialItemState[A <: TrivialItemState[A] & InventoryItem]
extends InventoryItemState:
  this: A =>

  protected type Self <: A
  //val companion: InventoryItemState.Companion[Self] with TrivialItemState.Companion[Self]
  //val companion: InventoryItemState.Companion[Self]
  //val item = this

  def toInitialItemState = this

  final def updateItem(item: companion.Item) =
    Right(item.toInitialItemState./*???*/asInstanceOf[companion.ItemState])

  override final def toSnapshotObservable =
    Observable.pure(item)


object TrivialItemState:
  trait Companion[A <: TrivialItemState[A] & InventoryItem]
  extends InventoryItemState.Companion[A] {
    //type Item = A
  }
