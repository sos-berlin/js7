package js7.data_for_java.agent

import io.vavr.control.{Either => VEither}
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data_for_java.common.JJsonable
import scala.jdk.OptionConverters._

final case class JAgentRefState(asScala: AgentRefState)
extends JJsonable[JAgentRefState]
{
  protected type AsScala = AgentRefState
  protected def companion = JAgentRefState

  @Nonnull
  def agentPath: AgentPath =
    asScala.agentPath

  @Nonnull
  def agentRef: JAgentRef =
    JAgentRef(asScala.agentRef)

  @Nonnull
  def problem: Optional[Problem] =
    asScala.problem.toJava
}

object JAgentRefState extends JJsonable.Companion[JAgentRefState]
{
  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JAgentRefState] =
    super.fromJson(jsonString)

  protected def jsonEncoder = AgentRefState.jsonCodec
  protected def jsonDecoder = AgentRefState.jsonCodec
}
