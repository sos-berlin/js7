package js7.subagent.data

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.Event
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.InventoryItem
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.subagent.SubagentState.inventoryItemJsonCodec

trait SubagentEvent extends Event.ForScala3[SubagentEvent] {
  val companion = SubagentEvent
}

object SubagentEvent extends Event.Companion[SubagentEvent]
{
  type Key = NoKey

  final case class SubagentDedicated(
    subagentId: SubagentId,
    subagentRunId: SubagentRunId,
    agentPath: AgentPath,
    controllerId: ControllerId)
  extends SubagentEvent

  final case class SubagentItemAttached(item: InventoryItem)
  extends SubagentEvent

  //final case class SubagentCommandsObserved(untilCommandId: Long)
  //extends SubagentEvent

  type SubagentShutdown = SubagentShutdown.type
  case object SubagentShutdown
  extends SubagentEvent

  implicit val jsonCodec = TypedJsonCodec[SubagentEvent](
    Subtype(deriveCodec[SubagentDedicated]),
    Subtype(deriveCodec[SubagentItemAttached]),
    //Subtype(deriveCodec[SubagentCommandsObserved]),
    Subtype(SubagentShutdown))

  intelliJuseImport(inventoryItemJsonCodec)
}
