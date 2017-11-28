package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.data.filebased.TypedPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath(string: String) extends TypedPath {
  validate()

  def companion = AgentPath
}

object AgentPath extends TypedPath.Companion[AgentPath] {

  override lazy val xmlFilenameExtension = ".agent.xml"
}
