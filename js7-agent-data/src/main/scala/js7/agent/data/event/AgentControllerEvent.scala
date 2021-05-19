package js7.agent.data.event

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.NoKeyEvent
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait AgentControllerEvent extends NoKeyEvent

object AgentControllerEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class AgentCreated(
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId)
  extends AgentControllerEvent

  final case class AgentReady(
    timezone: String,
    totalRunningTime: FiniteDuration)
  extends AgentControllerEvent

  implicit val jsonCodec = TypedJsonCodec[AgentControllerEvent](
    Subtype(deriveCodec[AgentCreated]),
    Subtype(deriveCodec[AgentReady]))
}
