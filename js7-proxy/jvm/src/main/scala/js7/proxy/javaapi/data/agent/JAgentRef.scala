package js7.proxy.javaapi.data.agent

import io.vavr.control.{Either => VEither}
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.proxy.javaapi.data.{JInventoryItem, JJsonable}

final case class JAgentRef(underlying: AgentRef)
extends JInventoryItem[JAgentRef, AgentRefPath]
{
  protected type Underlying = AgentRef
  protected def companion = JAgentRef

  def id = JAgentRefId(underlying.id)

  def path = underlying.path

  def uri: Uri = underlying.uri
}

object JAgentRef extends JJsonable.Companion[JAgentRef]
{
  override def fromJson(jsonString: String): VEither[Problem, JAgentRef] =
    super.fromJson(jsonString)

  def jsonEncoder = AgentRef.jsonEncoder
  def jsonDecoder = AgentRef.jsonDecoder
}
