package js7.data.item

trait UnsignedSimpleItem extends SimpleItem with UnsignedItem
{
  protected type Self <: UnsignedSimpleItem

  val companion: UnsignedSimpleItem.Companion[Self]
}

object UnsignedSimpleItem
{
  type Companion_ = Companion[_ <: UnsignedSimpleItem]

  trait Companion[A <: UnsignedSimpleItem]
  extends SimpleItem.Companion[A]
  with UnsignedItem.Companion[A]
  {
    type Key <: UnsignedSimpleItemPath
    val Key: UnsignedSimpleItemPath.Companion[Key]

    type ItemState <: UnsignedSimpleItemState
  }
}
