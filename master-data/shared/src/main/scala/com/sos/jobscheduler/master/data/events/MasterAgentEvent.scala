package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.Event

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterAgentEvent extends Event {
  type Key = AgentRefPath
}

object MasterAgentEvent
{
  final case class AgentCouplingFailed(message: String) extends MasterAgentEvent

  final case class AgentReady(timezone: String) extends MasterAgentEvent

  implicit val jsonCodec = TypedJsonCodec[MasterAgentEvent](
    Subtype(deriveCodec[AgentCouplingFailed]),
    Subtype(deriveCodec[AgentReady]))
}
