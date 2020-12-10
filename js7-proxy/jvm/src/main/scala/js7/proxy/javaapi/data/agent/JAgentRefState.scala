package js7.proxy.javaapi.data.agent

import io.vavr.control.{Either => VEither}
import js7.base.problem.Problem
import js7.controller.data.agent.AgentRefState
import js7.data.agent.AgentId
import js7.proxy.javaapi.data.common.JJsonable

final case class JAgentRefState(asScala: AgentRefState)
extends JJsonable[JAgentRefState]
{
  protected type AsScala = AgentRefState
  protected def companion = JAgentRefState

  def name: AgentId =
    asScala.name

  def agentRef: JAgentRef =
    JAgentRef(asScala.agentRef)
}

object JAgentRefState extends JJsonable.Companion[JAgentRefState]
{
  override def fromJson(jsonString: String): VEither[Problem, JAgentRefState] =
    super.fromJson(jsonString)

  protected def jsonEncoder = AgentRefState.jsonCodec
  protected def jsonDecoder = AgentRefState.jsonCodec
}
