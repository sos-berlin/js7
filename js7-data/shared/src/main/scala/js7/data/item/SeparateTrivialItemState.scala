package js7.data.item

import cats.effect.IO
import fs2.Stream
import js7.base.problem.Checked

/** For orthogonality, for Items with a still(!) separate empty ItemState. */
trait SeparateTrivialItemState[A <: SeparateTrivialItemState[A]]
extends InventoryItemState:

  this: A =>

  protected type Self <: InventoryItemState

  def path: item.companion.Path = item.path

  final def updateItem(item: companion.Item): Checked[companion.ItemState] =
    Right(item.toInitialItemState./*???*/asInstanceOf[companion.ItemState])

  override final def toSnapshotStream: Stream[IO, Any] =
    Stream.emit(item)


object SeparateTrivialItemState:
  trait Companion[A <: SeparateTrivialItemState[A]]
  extends InventoryItemState.Companion[A]
