package js7.data.agent

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.{DecodeWithDefaults, deriveConfiguredCodec}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.cluster.ClusterEvent
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, KeyedEvent, KeyedEventTypedJsonCodec}
import js7.data.platform.PlatformInfo

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentRefStateEvent extends Event {
  type Key = AgentPath
}

object AgentRefStateEvent
{
  /** A new Agent has been dedicated to this Controller. */
  final case class AgentDedicated(
    agentRunId: AgentRunId,
    agentEventId: Option[EventId]/*optional for compatibility with v2.0.0-RC3*/)
  extends AgentRefStateEvent

  /** Controller is coupled with Agent, ready for receiving events. */
  type AgentCoupled = AgentCoupled.type
  case object AgentCoupled extends AgentRefStateEvent

  final case class AgentCouplingFailed(problem: Problem) extends AgentRefStateEvent

  /** Agent is up and running. */
  final case class AgentReady(
    timezone: String/*COMPATIBLE with v2.3*/,
    platformInfo: Option/*COMPATIBLE with v2.3*/[PlatformInfo])
  extends AgentRefStateEvent

  final case class AgentEventsObserved(untilEventId: EventId) extends AgentRefStateEvent
  {
    override def isMinor = true
    override def toString = s"AgentEventsObserved(${EventId.toString(untilEventId)})"
  }

  final case class AgentResetStarted(force: Boolean = false)
  extends AgentRefStateEvent
  {
    override def toString = s"AgentResetStarted(force=$force)"
  }

  type AgentShutDown = AgentShutDown.type
  case object AgentShutDown extends AgentRefStateEvent

  type AgentReset = AgentReset.type
  case object AgentReset extends AgentRefStateEvent

  final case class AgentMirroredEvent(event: KeyedEvent[Event])
  extends AgentRefStateEvent
  object AgentMirroredEvent {
    private implicit val innerEventCodec: Codec.AsObject[KeyedEvent[Event]] =
      KeyedEventTypedJsonCodec[Event](
        KeyedSubtype[ClusterEvent])

    private[AgentRefStateEvent] implicit def jsonCodec: Codec.AsObject[AgentMirroredEvent] =
      deriveCodec[AgentMirroredEvent]
  }

  implicit val jsonCodec: TypedJsonCodec[AgentRefStateEvent] = TypedJsonCodec(
    Subtype(deriveConfiguredCodec[AgentDedicated], aliases = Seq("AgentCreated")),
    Subtype(AgentCoupled),
    Subtype(deriveConfiguredCodec[AgentCouplingFailed]),
    Subtype(deriveConfiguredCodec[AgentReady]),
    Subtype(deriveConfiguredCodec[AgentEventsObserved]),
    Subtype(AgentShutDown),
    Subtype(deriveConfiguredCodec[AgentResetStarted]),
    Subtype(AgentReset),
    Subtype[AgentMirroredEvent])

  intelliJuseImport(DecodeWithDefaults)
}
