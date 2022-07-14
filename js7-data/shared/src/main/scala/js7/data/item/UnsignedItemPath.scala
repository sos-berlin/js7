package js7.data.item

import io.circe.Codec
import js7.data.item.UnsignedItemPath.*

trait UnsignedItemPath extends InventoryItemPath
{
  protected type Self <: UnsignedItemPath

  def companion: Companion[? <: UnsignedItemPath]
}

object UnsignedItemPath
{
  trait Companion[A <: UnsignedItemPath]
  extends InventoryItemPath.Companion[A]
  {
    implicit def implicitCompanion = this
  }

  type AnyCompanion = Companion[? <: UnsignedItemPath]

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[UnsignedItemPath] =
    InventoryItemPath.jsonCodec(companions)
      .asInstanceOf[Codec[UnsignedItemPath]]
}
