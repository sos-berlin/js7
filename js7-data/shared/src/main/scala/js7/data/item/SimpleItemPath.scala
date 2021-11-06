package js7.data.item

import io.circe.Codec
import js7.data.item.SimpleItemPath._

trait SimpleItemPath extends InventoryItemKey with InventoryItemPath
{
  protected type Self <: SimpleItemPath

  def companion: Companion[_ <: SimpleItemPath]

  final def path = this
}

object SimpleItemPath
{
  trait Companion[A <: SimpleItemPath]
  extends InventoryItemPath.Companion[A]
  with InventoryItemKey.Companion[A]

  type AnyCompanion = Companion[_ <: SimpleItemPath]

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[SimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[SimpleItemPath]]
}
