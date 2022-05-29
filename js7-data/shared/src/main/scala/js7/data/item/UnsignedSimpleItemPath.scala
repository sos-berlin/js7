package js7.data.item

import io.circe.Codec
import js7.data.item.UnsignedSimpleItemPath._

trait UnsignedSimpleItemPath extends SimpleItemPath
{
  def companion: Companion[_ <: UnsignedSimpleItemPath]
}

object UnsignedSimpleItemPath
{
  type Companion_ = Companion[_ <: UnsignedSimpleItemPath]

  trait Companion[A <: UnsignedSimpleItemPath] extends SimpleItemPath.Companion[A]
  {
    type Item <: UnsignedSimpleItem
    override implicit def implicitCompanion = this
  }

  def jsonCodec(companions: Iterable[UnsignedSimpleItemPath.Companion_]): Codec[UnsignedSimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[UnsignedSimpleItemPath]]
}
