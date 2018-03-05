package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.data.agent.{AgentId, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Agent(id: AgentId, uri: String) extends FileBased
{
  type Self = Agent

  val companion = Agent

  def withId(id: FileBasedId[AgentPath]) = copy(id = id)
}

object Agent extends FileBased.Companion[Agent]
{
  type ThisFileBased = Agent
  type Path = AgentPath

  val typedPathCompanion = AgentPath
}
