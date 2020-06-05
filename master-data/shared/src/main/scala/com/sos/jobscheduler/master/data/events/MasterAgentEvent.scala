package js7.master.data.events

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.Event

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterAgentEvent extends Event {
  type Key = AgentRefPath
}

object MasterAgentEvent
{
  final case class AgentCouplingFailed(problem: Problem) extends MasterAgentEvent

  /** The Agent Server has registered the Master and has started a new Agent for the Master. */
  final case class AgentRegisteredMaster(agentRunId: AgentRunId) extends MasterAgentEvent
  object AgentRegisteredMaster {
    implicit val jsonCodec = deriveCodec[AgentRegisteredMaster]
  }

  final case class AgentReady(timezone: String) extends MasterAgentEvent

  implicit val jsonCodec = TypedJsonCodec[MasterAgentEvent](
    Subtype(deriveCodec[AgentCouplingFailed]),
    Subtype[AgentRegisteredMaster],
    Subtype(deriveCodec[AgentReady]))
}
