package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.event.Event
import io.circe.generic.JsonCodec

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
  @JsonCodec
  final case class AgentRegisteredMaster(agentRunId: AgentRunId) extends MasterAgentEvent

  final case class AgentReady(timezone: String) extends MasterAgentEvent

  implicit val jsonCodec = TypedJsonCodec[MasterAgentEvent](
    Subtype(deriveCodec[AgentCouplingFailed]),
    Subtype[AgentRegisteredMaster],
    Subtype(deriveCodec[AgentReady]))
}
