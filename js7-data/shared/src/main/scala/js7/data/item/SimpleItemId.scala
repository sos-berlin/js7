package js7.data.item

import io.circe.Codec
import js7.base.generic.GenericString
import js7.base.standards.Js7PathValidating
import js7.data.item.InventoryItemId.Companion

trait SimpleItemId extends InventoryItemId with GenericString
{
  protected type Self <: SimpleItemId

  def companion: Companion[_ <: SimpleItemId]

  final def toTypedString: String =
    s"${companion.itemTypeName}:$string"

  override def toString = toTypedString  // Used in some error messages
}

object SimpleItemId
{
  trait Companion[A <: SimpleItemId] extends InventoryItemId.Companion[A] with Js7PathValidating[A]

  type AnyCompanion = Companion[_ <: SimpleItemId]

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[SimpleItemId] =
    InventoryItemId.jsonCodec(companions)
      .asInstanceOf[Codec[SimpleItemId]]
}
