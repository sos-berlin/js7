package js7.data.item

import io.circe.Codec
import js7.base.generic.GenericString
import js7.base.standards.Js7PathValidating

trait SimpleItemId extends InventoryItemId with GenericString
{
  protected type Self <: SimpleItemId

  final def toTypedString: String =
    s"${companion.itemTypeName}:$string"
}

object SimpleItemId
{
  trait Companion[A <: SimpleItemId]
  extends InventoryItemId.Companion[A]
  with Js7PathValidating[A]

  type AnyCompanion = Companion[_ <: SimpleItemId]

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[SimpleItemId] =
    InventoryItemId.jsonCodec(companions)
      .asInstanceOf[Codec[SimpleItemId]]
}
