package js7.data.item

import cats.effect.IO
import fs2.Stream
import js7.base.problem.Checked

/** For orthogonality, for Items which are the (empty) ItemState. */
trait TrivialItemState[A <: TrivialItemState[A]]
extends InventoryItemState, InventoryItem:
  this: A =>

  protected type Self = A
  
  val companion: TrivialItemState.Companion[Self]
  val item = this

  def toInitialItemState: A = this

  final def updateItem(item: companion.Item): Checked[companion.ItemState] =
    Right(item.toInitialItemState)

  override final def toSnapshotStream: Stream[IO, Any] =
    Stream.emit(item)


object TrivialItemState:

  trait Companion[A <: TrivialItemState[A]]
  extends InventoryItemState.Companion[A], InventoryItem.Companion[A]:
    type Item = A
