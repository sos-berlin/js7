package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.data.event.{Event, EventId}

trait SubagentRefStateEvent extends Event.ForScala3[SubagentRefStateEvent]
{
  val companion = SubagentRefStateEvent
}

object SubagentRefStateEvent extends Event.Companion[SubagentRefStateEvent]
{
  type Key = SubagentId

  final case class SubagentDedicated(subagentRunId: SubagentRunId)
  extends SubagentRefStateEvent

  type SubagentCoupled = SubagentCoupled.type
  case object SubagentCoupled
  extends SubagentRefStateEvent

  final case class SubagentLost(problem: Problem)
  extends SubagentRefStateEvent

  final case class SubagentEventsObserved(untilEventId: EventId)
  extends SubagentRefStateEvent
  {
    override def toString = s"SubagentEventsObserved(${EventId.toString(untilEventId)})"
  }

  type SubagentReset = SubagentReset.type
  case object SubagentReset
  extends SubagentRefStateEvent

  implicit val jsonCodec = TypedJsonCodec[SubagentRefStateEvent](
    Subtype(deriveCodec[SubagentDedicated]),
    Subtype(SubagentCoupled),
    Subtype(deriveCodec[SubagentLost]),
    Subtype(deriveCodec[SubagentEventsObserved]),
    Subtype(SubagentReset))
}
