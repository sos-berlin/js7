package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath(string: String) extends TypedPath {

  def companion = AgentPath
}

object AgentPath extends TypedPath.Companion[AgentPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Xml → ".agent.xml")
}
