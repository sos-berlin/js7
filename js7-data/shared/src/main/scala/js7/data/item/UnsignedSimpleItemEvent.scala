package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.event.JournaledState

sealed trait UnsignedSimpleItemEvent extends InventoryItemEvent
{
  def key: SimpleItemPath
}

object UnsignedSimpleItemEvent
{
  sealed trait UnsignedSimpleItemAddedOrChanged extends UnsignedSimpleItemEvent {
    def item: SimpleItem
    assertThat(item.itemRevision.isDefined)
  }
  object UnsignedSimpleItemAddedOrChanged {
    def unapply(event: UnsignedSimpleItemAddedOrChanged) = Some(event.item)
  }

  final case class SimpleItemAdded(item: UnsignedSimpleItem)
  extends UnsignedSimpleItemAddedOrChanged
  {
    def key = item.key
  }

  final case class SimpleItemChanged(item: UnsignedSimpleItem)
  extends UnsignedSimpleItemAddedOrChanged
  {
    def key = item.key
  }

  def jsonCodec[S <: JournaledState[S]](implicit S: JournaledState.Companion[S])
  : TypedJsonCodec[UnsignedSimpleItemEvent] = {
    implicit val x = S.unsignedSimpleItemJsonCodec
    TypedJsonCodec(
      Subtype(deriveCodec[SimpleItemAdded]),
      Subtype(deriveCodec[SimpleItemChanged]))
  }
}
