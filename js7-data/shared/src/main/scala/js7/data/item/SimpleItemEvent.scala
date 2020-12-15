package js7.data.item

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

trait SimpleItemEvent extends ItemEvent
{
  def id: SimpleItemId
}

object SimpleItemEvent
{
  sealed trait SimpleItemAddedOrChanged extends SimpleItemEvent
  object SimpleItemAddedOrChanged {
    def unapply(item: SimpleItem) = Some(item)
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

  final case class SimpleItemDeleted(id: SimpleItemId)
  extends SimpleItemEvent

  def jsonCodec[A <: SimpleItem](companions: Seq[SimpleItem.Companion]): TypedJsonCodec[SimpleItemEvent] = {
    implicit val itemJsonCodec = SimpleItem.jsonCodec(companions)
    implicit val idJsonCodec = SimpleItemId.jsonCodec(companions.map(_.idCompanion))

    TypedJsonCodec(
      Subtype(deriveCodec[SimpleItemAdded]),
      Subtype(deriveCodec[SimpleItemChanged]),
      Subtype(deriveCodec[SimpleItemDeleted]))
  }
}
