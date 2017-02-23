package com.sos.jobscheduler.data.engine2.agent

import com.sos.jobscheduler.data.filebased.TypedPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath(string: String) extends TypedPath {
  validate()

  def companion = AgentPath
}

object AgentPath extends TypedPath.Companion[AgentPath] {

  override lazy val filenameExtension = ".agent.xml"
}
