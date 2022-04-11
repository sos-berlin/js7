package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.Event
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.InventoryItem
import js7.data.subagent.SubagentState.inventoryItemJsonCodec

trait SubagentEvent extends Event.ForScala3[SubagentEvent] {
  val companion = SubagentEvent
}

object SubagentEvent extends Event.Companion[SubagentEvent]
{
  type Key = NoKey

  final case class SubagentItemAttached(item: InventoryItem)
  extends SubagentEvent

  type SubagentShutdown = SubagentShutdown.type
  case object SubagentShutdown
  extends SubagentEvent

  implicit val jsonCodec = TypedJsonCodec[SubagentEvent](
    Subtype(deriveCodec[SubagentItemAttached]),
    Subtype(SubagentShutdown))

  intelliJuseImport(inventoryItemJsonCodec)
}
