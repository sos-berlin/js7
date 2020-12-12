package js7.proxy.javaapi.data.agent

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.{AgentId, AgentRef}
import js7.proxy.javaapi.data.common.JJsonable

final case class JAgentRef(asScala: AgentRef)
extends JJsonable[JAgentRef]
{
  protected type AsScala = AgentRef
  protected def companion = JAgentRef

  def name: AgentId =
    asScala.id

  @Nonnull
  def uri: Uri =
    asScala.uri
}

object JAgentRef extends JJsonable.Companion[JAgentRef]
{
  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JAgentRef] =
    super.fromJson(jsonString)

  protected def jsonEncoder = AgentRef.jsonCodec
  protected def jsonDecoder = AgentRef.jsonCodec
}
