package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath private(string: String) extends TypedPath {

  def companion = AgentPath
}

object AgentPath extends TypedPath.Companion[AgentPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json → ".agent.json",
    SourceType.Yaml → ".agent.yaml",
    SourceType.Xml → ".agent.xml")

  protected def unchecked(string: String) = new AgentPath(string)
}
