package js7.subagent.data

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId}
import js7.data.item.InventoryItem
import js7.subagent.SubagentState.inventoryItemJsonCodec

trait SubagentEvent extends Event.ForScala3[SubagentEvent] {
  val companion = SubagentEvent
}

object SubagentEvent extends Event.Companion[SubagentEvent]
{
  type Key = NoKey

  case object SubagentCoupled
  extends SubagentEvent

  final case class SubagentCouplingFailed(problem: Problem)
  extends SubagentEvent

  final case class SubagentItemAttached(item: InventoryItem)
  extends SubagentEvent

  final case class SubagentCommandsObserved(untilCommandId: Long)
  extends SubagentEvent

  final case class SubagentEventsObserved(untilEventId: EventId)
  extends SubagentEvent
  {
    override def toString =
      s"SubagentEventsObserved(${EventId.toString(untilEventId)})"
  }

  implicit val jsonCodec = TypedJsonCodec[SubagentEvent](
    Subtype(SubagentCoupled),
    Subtype(deriveCodec[SubagentCouplingFailed]),
    Subtype(deriveCodec[SubagentItemAttached]),
    Subtype(deriveCodec[SubagentEventsObserved]),
    Subtype(deriveCodec[SubagentCommandsObserved]))

  intelliJuseImport(inventoryItemJsonCodec)
}
