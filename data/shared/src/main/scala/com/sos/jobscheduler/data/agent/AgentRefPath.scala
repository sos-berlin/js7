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
    SourceType.Json -> ".agent.json",
    SourceType.Yaml -> ".agent.yaml",
    SourceType.Xml -> ".agent.xml")

  protected def unchecked(string: String) = new AgentRefPath(string)
}
