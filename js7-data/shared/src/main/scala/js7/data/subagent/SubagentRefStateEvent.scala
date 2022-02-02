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

  /** Subagent has been named. */
  final case class SubagentDedicated(subagentRunId: SubagentRunId)
  extends SubagentRefStateEvent

  type SubagentCoupled = SubagentCoupled.type
  /** Subagent is coupled and alive. */
  case object SubagentCoupled
  extends SubagentRefStateEvent

  /** Subagent has been lost and its state is unknown. May recouple later. */
  final case class SubagentLost(problem: Problem)
  extends SubagentRefStateEvent

  /** Subagent should delete events until `untilEventId`. */
  final case class SubagentEventsObserved(untilEventId: EventId)
  extends SubagentRefStateEvent
  {
    override def toString = s"SubagentEventsObserved(${EventId.toString(untilEventId)})"
  }

  type SubagentReset = SubagentReset.type
  /** Subagent has lost its state (including processes). */
  case object SubagentReset
  extends SubagentRefStateEvent

  type SubagentShutdown = SubagentShutdown.type
  case object SubagentShutdown
  extends SubagentRefStateEvent

  implicit val jsonCodec = TypedJsonCodec[SubagentRefStateEvent](
    Subtype(deriveCodec[SubagentDedicated]),
    Subtype(SubagentCoupled),
    Subtype(deriveCodec[SubagentLost]),
    Subtype(deriveCodec[SubagentEventsObserved]),
    Subtype(SubagentReset),
    Subtype(SubagentShutdown))
}
