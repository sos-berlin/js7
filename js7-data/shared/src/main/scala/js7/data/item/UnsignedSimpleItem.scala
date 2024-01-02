package js7.data.item

trait UnsignedSimpleItem extends SimpleItem, UnsignedItem:
  protected type Self <: UnsignedSimpleItem

  val companion: UnsignedSimpleItem.Companion[Self]


object UnsignedSimpleItem:
  type Companion_ = Companion[? <: UnsignedSimpleItem]

  trait Companion[A <: UnsignedSimpleItem]
  extends SimpleItem.Companion[A], UnsignedItem.Companion[A]:
    type Key <: UnsignedSimpleItemPath
    def Key: UnsignedSimpleItemPath.Companion[Key]

    type ItemState <: UnsignedSimpleItemState
