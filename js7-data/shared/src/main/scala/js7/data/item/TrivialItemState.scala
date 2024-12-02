package js7.data.item

import fs2.{Pure, Stream}
import js7.base.problem.Checked

/** For orthogonality, an InventoryItem which is also an (empty) InventoryItemState. */
trait TrivialItemState[A <: TrivialItemState[A]]
extends InventoryItemState, InventoryItem:
  this: A =>

  protected type Self = A

  val companion: TrivialItemState.Companion[Self]
  val item: A = this

  def toInitialItemState: A = this

  final def updateItem(item: companion.Item): Checked[companion.ItemState] =
    Right(item.toInitialItemState)

  override final def toSnapshotStream: Stream[Pure, A] =
    Stream.emit(item) // This InventoryItem is the InventoryItemState


object TrivialItemState:

  trait Companion[A <: TrivialItemState[A]]
  extends InventoryItemState.Companion[A], InventoryItem.Companion[A]:
    type Item = A
    type ItemState = A
