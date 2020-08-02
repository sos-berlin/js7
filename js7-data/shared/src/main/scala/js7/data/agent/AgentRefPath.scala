package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.filebased.{SourceType, TypedPath}

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

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validPath: String) = apply(validPath)
}
