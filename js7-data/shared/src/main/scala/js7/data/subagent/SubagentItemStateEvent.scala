package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.data.event.{Event, EventId}
import js7.data.platform.PlatformInfo

trait SubagentItemStateEvent extends Event.IsKeyBase[SubagentItemStateEvent]:
  val keyCompanion: SubagentItemStateEvent.type = SubagentItemStateEvent


object SubagentItemStateEvent extends Event.CompanionForKey[SubagentId, SubagentItemStateEvent]:
  implicit def implicitSelf: SubagentItemStateEvent.type  = this

  /** Subagent has been named. */
  final case class SubagentDedicated(
    subagentRunId: SubagentRunId,
    platformInfo: Option[PlatformInfo])
  extends SubagentItemStateEvent:
    override def toShortString = s"SubagentDedicated($subagentRunId)"

  type SubagentCoupled = SubagentCoupled.type
  /** Subagent is coupled and alive. */
  case object SubagentCoupled
  extends SubagentItemStateEvent

  /** Subagent has been lost and its state is unknown. May recouple later. */
  final case class SubagentCouplingFailed(problem: Problem)
  extends SubagentItemStateEvent

  /** Subagent may delete events until `untilEventId`. */
  final case class SubagentEventsObserved(untilEventId: EventId)
  extends SubagentItemStateEvent:
    override def isMinor = true
    override def toString = s"SubagentEventsObserved(${EventId.toString(untilEventId)})"

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

  case object SubagentShutdownV7 // COMPATIBLE with v2.7, don't use this
  extends SubagentDied

  type SubagentShutdown = SubagentShutdown.type
  case object SubagentShutdown
  extends SubagentDied

  implicit val jsonCodec: TypedJsonCodec[SubagentItemStateEvent] = TypedJsonCodec(
    Subtype(deriveCodec[SubagentDedicated]),
    Subtype(SubagentCoupled),
    Subtype(deriveCodec[SubagentCouplingFailed]),
    Subtype(deriveCodec[SubagentEventsObserved]),
    Subtype(deriveCodec[SubagentResetStartedByController]),
    Subtype(deriveCodec[SubagentResetStarted]),
    Subtype(SubagentReset),
    Subtype(SubagentRestarted),
    Subtype.renamedSingleton(SubagentShutdownV7, "SubagentShutdown"),
    Subtype.renamedSingleton(SubagentShutdown, "SubagentShutdown2"))
