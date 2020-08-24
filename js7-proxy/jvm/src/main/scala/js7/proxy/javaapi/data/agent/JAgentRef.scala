package js7.proxy.javaapi.data.agent

import io.vavr.control.{Either => VEither}
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.item.JInventoryItem

final case class JAgentRef(asScala: AgentRef)
extends JInventoryItem[JAgentRef, AgentRefPath]
{
  protected type AsScala = AgentRef
  protected def companion = JAgentRef

  def id = JAgentRefId(asScala.id)

  def path = asScala.path

  def uri: Uri = asScala.uri
}

object JAgentRef extends JJsonable.Companion[JAgentRef]
{
  override def fromJson(jsonString: String): VEither[Problem, JAgentRef] =
    super.fromJson(jsonString)

  def jsonEncoder = AgentRef.jsonEncoder
  def jsonDecoder = AgentRef.jsonDecoder
}
