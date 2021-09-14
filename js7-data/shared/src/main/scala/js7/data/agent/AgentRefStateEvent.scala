package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.data.event.{Event, EventId}

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentRefStateEvent extends Event {
  type Key = AgentPath
}

object AgentRefStateEvent
{
  /** A new Agent has been created and is running. */
  final case class AgentCreated(
    agentRunId: AgentRunId,
    agentEventId: Option[EventId]/*optional for compatibility with v2.0.0-RC3*/)
  extends AgentRefStateEvent

  final case class AgentCouplingFailed(problem: Problem) extends AgentRefStateEvent

  final case class AgentReady(timezone: String) extends AgentRefStateEvent

  final case class AgentEventsObserved(untilEventId: EventId) extends AgentRefStateEvent
  {
    override def toString = s"AgentEventsObserved(${EventId.toString(untilEventId)})"
  }

  case object AgentResetStarted extends AgentRefStateEvent

  type AgentShutDown = AgentShutDown.type
  case object AgentShutDown extends AgentRefStateEvent

  type AgentReset = AgentReset.type
  case object AgentReset extends AgentRefStateEvent

  implicit val jsonCodec = TypedJsonCodec[AgentRefStateEvent](
    Subtype(deriveCodec[AgentCreated]),
    Subtype(deriveCodec[AgentCouplingFailed]),
    Subtype(deriveCodec[AgentReady]),
    Subtype(deriveCodec[AgentEventsObserved]),
    Subtype(AgentShutDown),
    Subtype(AgentResetStarted),
    Subtype(AgentReset))
}
