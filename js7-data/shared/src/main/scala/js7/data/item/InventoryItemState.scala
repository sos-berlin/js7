package js7.data.item

import cats.effect.IO
import fs2.{Pure, Stream}
import js7.base.problem.Checked

trait InventoryItemState:

  protected type Self <: InventoryItemState

  val companion: InventoryItemState.Companion[Self]
  val item: companion.Item

  def updateItem(item: companion.Item): Checked[companion.ItemState]

  def toSnapshotStream: Stream[fs2.Pure, Any] =
    Stream.emit(this)

  def toStringStream: Stream[Pure, String] =
    Stream.emit(toString)


object InventoryItemState:
  type AnyCompanion = Companion[? <: InventoryItemState]

  trait Companion[A <: InventoryItemState]:
    type Key <: InventoryItemKey
    type Path <: InventoryItemPath
    type Item <: InventoryItem
    type ItemState = A
