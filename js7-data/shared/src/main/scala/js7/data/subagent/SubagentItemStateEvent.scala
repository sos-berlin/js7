package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.version.Version
import js7.data.event.{Event, EventId}

trait SubagentItemStateEvent extends Event.ForScala3[SubagentItemStateEvent]
{
  val companion = SubagentItemStateEvent
}

object SubagentItemStateEvent extends Event.Companion[SubagentItemStateEvent]
{
  type Key = SubagentId

  /** Subagent has been named. */
  final case class SubagentDedicated(subagentRunId: SubagentRunId, version: Option[Version])
  extends SubagentItemStateEvent

  type SubagentCoupled = SubagentCoupled.type
  /** Subagent is coupled and alive. */
  case object SubagentCoupled
  extends SubagentItemStateEvent

  /** Subagent has been lost and its state is unknown. May recouple later. */
  final case class SubagentCouplingFailed(problem: Problem)
  extends SubagentItemStateEvent

  /** Subagent may delete events until `untilEventId`. */
  final case class SubagentEventsObserved(untilEventId: EventId)
  extends SubagentItemStateEvent
  {
    override def toString = s"SubagentEventsObserved(${EventId.toString(untilEventId)})"
  }

  // TODO Brauchen wir SubagentCouplingFailed und SubagentReset ?

  sealed trait SubagentDied extends SubagentItemStateEvent

  final case class SubagentResetStartedByController(force: Boolean)
  extends SubagentItemStateEvent

  final case class SubagentResetStarted(force: Boolean)
  extends SubagentItemStateEvent

  type SubagentReset = SubagentReset.type
  case object SubagentReset
  extends SubagentDied

  type SubagentRestarted = SubagentRestarted.type
  /** Subagent has lost its state (including processes). */
  case object SubagentRestarted
  extends SubagentDied

  type SubagentShutdown = SubagentShutdown.type
  case object SubagentShutdown
  extends SubagentDied

  implicit val jsonCodec = TypedJsonCodec[SubagentItemStateEvent](
    Subtype(deriveCodec[SubagentDedicated]),
    Subtype(SubagentCoupled),
    Subtype(deriveCodec[SubagentCouplingFailed]),
    Subtype(deriveCodec[SubagentEventsObserved]),
    Subtype(deriveCodec[SubagentResetStartedByController]),
    Subtype(deriveCodec[SubagentResetStarted]),
    Subtype(SubagentReset),
    Subtype(SubagentRestarted),
    Subtype(SubagentShutdown))
}
