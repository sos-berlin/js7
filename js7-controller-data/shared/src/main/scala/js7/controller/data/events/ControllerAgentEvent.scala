package js7.controller.data.events

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.Event

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerAgentEvent extends Event {
  type Key = AgentRefPath
}

object ControllerAgentEvent
{
  final case class AgentCouplingFailed(problem: Problem) extends ControllerAgentEvent

  /** The Agent Server has registered the Controller and has started a new Agent for the Controller. */
  final case class AgentRegisteredController(agentRunId: AgentRunId) extends ControllerAgentEvent
  object AgentRegisteredController {
    implicit val jsonCodec = deriveCodec[AgentRegisteredController]
  }

  final case class AgentReady(timezone: String) extends ControllerAgentEvent

  implicit val jsonCodec = TypedJsonCodec[ControllerAgentEvent](
    Subtype(deriveCodec[AgentCouplingFailed]),
    Subtype[AgentRegisteredController],
    Subtype(deriveCodec[AgentReady]))
}
