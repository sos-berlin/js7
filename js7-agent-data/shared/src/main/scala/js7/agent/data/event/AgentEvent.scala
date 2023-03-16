package js7.agent.data.event

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.NoKeyEvent
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentId
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait AgentEvent extends NoKeyEvent

object AgentEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  /** Agent has been dedicated to a Controller. */
  final case class AgentDedicated(
    subagentId: Option[SubagentId],
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId)
  extends AgentEvent

  /** Agent is up and running. */
  final case class AgentReady(
    timezone: String,
    totalRunningTime: FiniteDuration,
    platformInfo: Option/*COMPATIBLE with v2.3*/[PlatformInfo])
  extends AgentEvent

  type AgentShutDown = AgentShutDown.type
  case object AgentShutDown extends AgentEvent

  implicit val jsonCodec: TypedJsonCodec[AgentEvent] = TypedJsonCodec(
    Subtype(deriveCodec[AgentDedicated], aliases = Seq("AgentCreated")),
    Subtype(deriveCodec[AgentReady]),
    Subtype(AgentShutDown))
}
