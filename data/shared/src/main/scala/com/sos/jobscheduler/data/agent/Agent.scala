package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}

/**
  * @author Joacim Zschimmer
  */
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
  implicit val jsonCodec = deriveCodec[Agent]

  override val self = this
  implicit val fileBasedsOverview = AgentsOverview
  val typedPathCompanion = AgentPath
}
