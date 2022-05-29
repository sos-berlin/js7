package js7.data.item

import js7.base.circeutils.typed.TypedJsonCodec

trait UnsignedSimpleItem extends SimpleItem
{
  protected type Self <: UnsignedSimpleItem

  val companion: UnsignedSimpleItem.Companion[Self]
}

object UnsignedSimpleItem
{
  type Companion_ = Companion[_ <: UnsignedSimpleItem]

  trait Companion[A <: UnsignedSimpleItem] extends SimpleItem.Companion[A]
  {
    type Key <: UnsignedSimpleItemPath
    val Key: UnsignedSimpleItemPath.Companion[Key]
  }

  private def jsonCodec(companions: Seq[Companion_]) =
    TypedJsonCodec.fromIterable("UnsignedSimpleItem", companions.map(_.subtype))
}
