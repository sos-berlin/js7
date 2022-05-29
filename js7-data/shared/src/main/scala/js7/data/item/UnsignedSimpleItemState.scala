package js7.data.item

trait UnsignedSimpleItemState extends SimpleItemState
{
  protected type Self <: UnsignedSimpleItemState
  val companion: UnsignedSimpleItemState.Companion[Self]
  protected type Item <: UnsignedSimpleItem

  val item: Item
}

object UnsignedSimpleItemState
{
  trait Companion[A <: UnsignedSimpleItemState] extends SimpleItemState.Companion[A]
  {
    type Path <: UnsignedSimpleItemPath
  }
}
