package  js7.data_for_java.agent

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem

final case class JAgentRef(asScala: AgentRef)
extends JJsonable[JAgentRef] with JUnsignedSimpleItem
{
  protected type AsScala = AgentRef
  protected def companion = JAgentRef

  @Nonnull
  def path: AgentPath =
    asScala.path

  @Nonnull
  def uri: Uri =
    asScala.uri
}

object JAgentRef extends JJsonable.Companion[JAgentRef]
{
  @Nonnull
  def of(@Nonnull path: AgentPath, @Nonnull uri: Uri) =
    JAgentRef(AgentRef(path, uri))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JAgentRef] =
    super.fromJson(jsonString)

  protected def jsonEncoder = AgentRef.jsonCodec
  protected def jsonDecoder = AgentRef.jsonCodec
}
