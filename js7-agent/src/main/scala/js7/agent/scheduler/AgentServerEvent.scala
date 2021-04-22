package js7.agent.scheduler

import js7.base.circeutils.CirceUtils
import js7.base.circeutils.JavaJsonCodecs.zoneIdJsonEncoder
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.NoKeyEvent

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentServerEvent extends NoKeyEvent

object AgentServerEvent
{
  intelliJuseImport(zoneIdJsonEncoder)

  final case class ControllerRegistered(
    controllerId: ControllerId,
    agentId: AgentPath,
    agentRunId: AgentRunId)
  extends AgentServerEvent

  implicit val jsonCodec: TypedJsonCodec[AgentServerEvent] = TypedJsonCodec[AgentServerEvent](
    Subtype(CirceUtils.deriveCodec[ControllerRegistered])
  )
}
