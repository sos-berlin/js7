package js7.data.item

trait UnsignedSimpleItemState extends SimpleItemState
{
  protected type Self <: UnsignedSimpleItemState
  val companion: UnsignedSimpleItemState.Companion[Self]

  val item: companion.Item
  def path: item.companion.Path
}

object UnsignedSimpleItemState
{
  trait Companion[A <: UnsignedSimpleItemState] extends SimpleItemState.Companion[A]
  {
    type Key = Path
    type Path <: UnsignedSimpleItemPath
    type ItemState <: UnsignedSimpleItemState
    type Item <: UnsignedSimpleItem
  }
}
