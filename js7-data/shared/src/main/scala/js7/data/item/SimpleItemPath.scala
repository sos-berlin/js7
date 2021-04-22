package js7.data.item

import io.circe.Codec
import js7.base.generic.GenericString
import js7.base.standards.Js7PathValidating
import js7.data.item.InventoryItemKey.Companion

trait SimpleItemPath extends InventoryItemKey with GenericString
{
  protected type Self <: SimpleItemPath

  def companion: Companion[_ <: SimpleItemPath]

  final def toTypedString: String =
    s"${companion.itemTypeName}:$string"

  override def toString = toTypedString  // Used in some error messages
}

object SimpleItemPath
{
  trait Companion[A <: SimpleItemPath] extends InventoryItemKey.Companion[A] with Js7PathValidating[A]

  type AnyCompanion = Companion[_ <: SimpleItemPath]

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[SimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[SimpleItemPath]]
}
