package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.event.ItemContainer

sealed trait UnsignedSimpleItemEvent extends InventoryItemEvent
{
  def key: SimpleItemPath
}

object UnsignedSimpleItemEvent
{
  sealed trait UnsignedSimpleItemAddedOrChanged extends UnsignedSimpleItemEvent with ItemAddedOrChanged {
    def item: SimpleItem
    assertThat(item.itemRevision.isDefined)
  }
  object UnsignedSimpleItemAddedOrChanged {
    def unapply(event: UnsignedSimpleItemAddedOrChanged) = Some(event.item)
  }

  final case class UnsignedSimpleItemAdded(item: UnsignedSimpleItem)
  extends UnsignedSimpleItemAddedOrChanged
  {
    def key = item.key
  }

  final case class UnsignedSimpleItemChanged(item: UnsignedSimpleItem)
  extends UnsignedSimpleItemAddedOrChanged
  {
    def key = item.key
  }

  def jsonCodec[S](implicit S: ItemContainer.Companion[S])
  : TypedJsonCodec[UnsignedSimpleItemEvent] = {
    implicit val x = S.unsignedSimpleItemJsonCodec
    TypedJsonCodec(
      Subtype(deriveCodec[UnsignedSimpleItemAdded]),
      Subtype(deriveCodec[UnsignedSimpleItemChanged]))
  }
}
