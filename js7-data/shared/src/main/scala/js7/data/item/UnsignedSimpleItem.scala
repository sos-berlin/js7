package js7.data.item

trait UnsignedSimpleItem extends SimpleItem
{
  protected type Self <: UnsignedSimpleItem

  val companion: UnsignedSimpleItem.Companion[Self]

  def toInitialItemState: companion.ItemState
}

object UnsignedSimpleItem
{
  type Companion_ = Companion[_ <: UnsignedSimpleItem]

  trait Companion[A <: UnsignedSimpleItem] extends SimpleItem.Companion[A]
  {
    type Key <: UnsignedSimpleItemPath
    val Key: UnsignedSimpleItemPath.Companion[Key]

    type ItemState <: UnsignedSimpleItemState
  }
}
