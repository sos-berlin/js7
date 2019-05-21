package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.master.MasterId
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
private[scheduler] sealed trait AgentSnapshot

private[scheduler] object AgentSnapshot {
  @JsonCodec
  final case class Master(masterId: MasterId, agentRunId: AgentRunId) extends AgentSnapshot

  val jsonCodec = TypedJsonCodec[Any](
    Subtype[Master])
}
