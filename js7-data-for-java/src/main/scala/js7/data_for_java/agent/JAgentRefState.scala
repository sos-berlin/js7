package js7.data_for_java.agent

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data.platform.PlatformInfo
import js7.data_for_java.cluster.JClusterState
import js7.data_for_java.common.JJsonable
import scala.jdk.OptionConverters.*

final case class JAgentRefState(asScala: AgentRefState)
extends JJsonable[JAgentRefState]:
  type AsScala = AgentRefState
  protected def companion = JAgentRefState

  @Nonnull
  def agentPath: AgentPath =
    asScala.agentPath

  @Nonnull
  def agentRef: JAgentRef =
    JAgentRef(asScala.agentRef)

  @Nonnull
  def timezoneString: Optional[String] =
    asScala.timezone.toJava

  @Nonnull
  def platformInfo: Optional[PlatformInfo] =
    asScala.platformInfo.toJava

  @Nonnull
  def problem: Optional[Problem] =
    asScala.problem.toJava

  @Nonnull
  def clusterState: JClusterState =
    JClusterState(asScala.clusterState)

object JAgentRefState extends JJsonable.Companion[JAgentRefState]:
  type AsScala = AgentRefState

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JAgentRefState] =
    super.fromJson(jsonString)

  protected def jsonEncoder = AgentRefState.jsonEncoder
  protected def jsonDecoder = AgentRefState.jsonDecoder
