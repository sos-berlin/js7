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
    type Id <: UnsignedSimpleItemPath
    val Id: UnsignedSimpleItemPath.Companion[Id]
  }

  def jsonCodec[A <: UnsignedSimpleItem](companions: Seq[Companion_]) =
    TypedJsonCodec.fromIterable(companions.map(_.subtype))
}
