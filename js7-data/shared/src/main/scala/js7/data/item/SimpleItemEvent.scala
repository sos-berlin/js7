package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat

sealed trait SimpleItemEvent extends InventoryItemEvent
{
  def id: SimpleItemId
}

object SimpleItemEvent
{
  sealed trait SimpleItemAddedOrChanged extends SimpleItemEvent {
    def item: SimpleItem
    assertThat(item.itemRevision.isDefined)
  }
  object SimpleItemAddedOrChanged {
    def unapply(event: SimpleItemAddedOrChanged) = Some(event.item)
  }

  final case class SimpleItemAdded(item: SimpleItem)
  extends SimpleItemAddedOrChanged
  {
    def id = item.id
  }

  final case class SimpleItemChanged(item: SimpleItem)
  extends SimpleItemAddedOrChanged
  {
    def id = item.id
  }

  def jsonCodec[A <: SimpleItem](companions: Seq[SimpleItem.Companion]): TypedJsonCodec[SimpleItemEvent] = {
    implicit val itemJsonCodec = SimpleItem.jsonCodec(companions)
    implicit val idJsonCodec = SimpleItemId.jsonCodec(companions.map(_.Id))

    TypedJsonCodec(
      Subtype(deriveCodec[SimpleItemAdded]),
      Subtype(deriveCodec[SimpleItemChanged]))
  }
}
