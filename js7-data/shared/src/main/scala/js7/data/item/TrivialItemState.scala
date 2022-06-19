package js7.data.item

import monix.reactive.Observable

/** For orthogonality, for Items without a state. */
trait TrivialItemState extends UnsignedSimpleItemState {
  protected type Self <: UnsignedSimpleItemState

  final def updateItem(item: companion.Item) =
    Right(item.toInitialItemState./*???*/asInstanceOf[companion.ItemState])

  override final def toSnapshotObservable =
    Observable.pure(item)
}
