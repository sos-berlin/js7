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
  final case class AgentCouplingFailed(problem: Problem) extends AgentRefStateEvent

  /** The Agent has registered the Controller and has started a new Agent for the Controller. */
  final case class AgentRegisteredController(agentRunId: AgentRunId) extends AgentRefStateEvent
  object AgentRegisteredController {
    implicit val jsonCodec = deriveCodec[AgentRegisteredController]
  }

  final case class AgentReady(timezone: String) extends AgentRefStateEvent

  final case class AgentEventsObserved(untilEventId: EventId) extends AgentRefStateEvent
  {
    override def toString = s"AgentEventsObserved(${EventId.toString(untilEventId)})"
  }

  implicit val jsonCodec = TypedJsonCodec[AgentRefStateEvent](
    Subtype(deriveCodec[AgentCouplingFailed]),
    Subtype[AgentRegisteredController],
    Subtype(deriveCodec[AgentReady]),
    Subtype(deriveCodec[AgentEventsObserved]))
}
