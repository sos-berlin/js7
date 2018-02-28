package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Agent(path: AgentPath, uri: String) extends FileBased {
  type Self = Agent
  def companion = Agent
}

object Agent extends FileBased.Companion[Agent]
{
  type ThisFileBased = Agent
  type ThisTypedPath = AgentPath

  val typedPathCompanion = AgentPath
}
