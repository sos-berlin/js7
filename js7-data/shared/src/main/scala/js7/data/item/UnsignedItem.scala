package js7.data.item

import js7.data.item.UnsignedItem.Companion

trait UnsignedItem extends InventoryItem:
  protected type Self <: UnsignedItem

  val companion: Companion[Self]

  def key: companion.Key


object UnsignedItem:
  type Companion_ = Companion[? <: UnsignedItem]

  trait Companion[A <: UnsignedItem] extends InventoryItem.Companion[A]:
    type Item <: A
    def cls: Class[A]

    type Key <: UnsignedItemKey
    def Key: UnsignedItemKey.Companion[Key]

    type Path <: UnsignedItemPath
    val Path: UnsignedItemPath.Companion[Path]

    type ItemState <: UnsignedItemState
