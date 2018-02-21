package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Agent(path: AgentPath, uri: String) extends FileBased

object Agent extends FileBased.Companion {
  type ThisFileBased = Agent
  type ThisTypedPath = AgentPath

  val typedPathCompanion = AgentPath
}
