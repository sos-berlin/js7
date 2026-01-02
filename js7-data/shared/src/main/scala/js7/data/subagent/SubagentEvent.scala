package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.NoKeyEvent
import js7.data.item.InventoryItem
import js7.data.subagent.SubagentState.inventoryItemJsonCodec

trait SubagentEvent extends NoKeyEvent


object SubagentEvent :
  final case class SubagentItemAttached(item: InventoryItem)
  extends SubagentEvent:
    override def toShortString = s"$SubagentItemAttached(${item.key})"


  type SubagentShutdownStarted = SubagentShutdownStarted.type
  case object SubagentShutdownStarted
  extends SubagentEvent

  type SubagentShutdown = SubagentShutdown.type
  case object SubagentShutdown
  extends SubagentEvent

  implicit val jsonCodec: TypedJsonCodec[SubagentEvent] = TypedJsonCodec(
    Subtype(deriveCodec[SubagentItemAttached]),
    Subtype(SubagentShutdownStarted),
    Subtype(SubagentShutdown))

  intelliJuseImport(inventoryItemJsonCodec)
