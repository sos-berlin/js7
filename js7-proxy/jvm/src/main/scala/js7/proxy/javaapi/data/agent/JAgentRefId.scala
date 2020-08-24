package js7.proxy.javaapi.data.agent

import io.vavr.control.{Either => VEither}
import js7.base.problem.Problem
import js7.data.agent.{AgentRefId, AgentRefPath}
import js7.data.item.{ItemId, VersionId}
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.item.JItemId

final case class JAgentRefId(asScala: AgentRefId)
extends JJsonable[JAgentRefId] with JItemId[AgentRefPath]
{
  protected type AsScala = AgentRefId
  protected type ScalaPath = AgentRefPath
  protected def companion = JAgentRefId

  def path = asScala.path
}

object JAgentRefId extends JJsonable.Companion[JAgentRefId]
{
  @throws[RuntimeException]("on invalid syntax")
  def of(path: String, versionId: String): JAgentRefId =
    JAgentRefId(AgentRefPath(path) ~ versionId)

  def of(path: AgentRefPath, versionId: VersionId): JAgentRefId =
    JAgentRefId(path ~ versionId)

  override def fromJson(jsonString: String): VEither[Problem, JAgentRefId] =
    super.fromJson(jsonString)

  val jsonEncoder = ItemId.jsonEncoder[AgentRefPath]
  val jsonDecoder = ItemId.jsonDecoder[AgentRefPath]
}
