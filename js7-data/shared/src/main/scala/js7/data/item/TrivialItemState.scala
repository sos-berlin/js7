package js7.data.item

import cats.effect.IO
import fs2.Stream
import js7.base.problem.Checked

/** For orthogonality, for Items which are the (empty) ItemState. */
trait TrivialItemState[A <: TrivialItemState[A] & InventoryItem]
extends InventoryItemState:
  this: A =>

  protected type Self <: A
  //val companion: InventoryItemState.Companion[Self] with TrivialItemState.Companion[Self]
  //val companion: InventoryItemState.Companion[Self]
  //val item = this

  def toInitialItemState: A = this

  final def updateItem(item: companion.Item): Checked[companion.ItemState] =
    Right(item.toInitialItemState./*???*/asInstanceOf[companion.ItemState])

  override final def toSnapshotStream: Stream[IO, Any] =
    Stream.emit(item)


object TrivialItemState:
  trait Companion[A <: TrivialItemState[A] & InventoryItem]
  extends InventoryItemState.Companion[A] {
    //type Item = A
  }
