package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class AgentRefPath private(string: String) extends TypedPath {

  def companion = AgentRefPath
}

object AgentRefPath extends TypedPath.Companion[AgentRefPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json -> ".agentref.json",
    SourceType.Yaml -> ".agentref.yaml",
    SourceType.Xml -> ".agent.xml")

  protected def unchecked(string: String) = new AgentRefPath(string)
}
