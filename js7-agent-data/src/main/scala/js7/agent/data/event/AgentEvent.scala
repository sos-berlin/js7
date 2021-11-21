package js7.agent.data.event

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.NoKeyEvent
import js7.data.subagent.SubagentId
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait AgentEvent extends NoKeyEvent

object AgentEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  /** Agent has been dedicated to a Controlller. */
  final case class AgentDedicated(
    subagentId: Option[SubagentId],
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId)
  extends AgentEvent

  /** Agent is up and running. */
  final case class AgentReady(
    timezone: String,
    totalRunningTime: FiniteDuration)
  extends AgentEvent

  type AgentShutDown = AgentShutDown.type
  case object AgentShutDown extends AgentEvent

  implicit val jsonCodec = TypedJsonCodec[AgentEvent](
    Subtype(deriveCodec[AgentDedicated], aliases = Seq("AgentCreated")),
    Subtype(deriveCodec[AgentReady]),
    Subtype(AgentShutDown))
}
