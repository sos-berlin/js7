package js7.agent.data.event

import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.{Decoder, Encoder}
import js7.agent.data.event
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.NoKeyEvent
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentId
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait AgentEvent extends NoKeyEvent

object AgentEvent:

  /** Agent has been dedicated to a Controller. */
  final case class AgentDedicated(
    agentPath: AgentPath,
    directors: Seq[SubagentId],
    agentRunId: AgentRunId,
    controllerId: ControllerId,
    controllerRunId: Option[ControllerRunId]/*COMPATIBLE None until v2.5, Some since v2.6*/)
  extends AgentEvent
  object AgentDedicated:
    implicit val jsonEncoder: Encoder.AsObject[AgentDedicated] = deriveEncoder
    implicit val jsonDecoder: Decoder[AgentDedicated] =
      c => for
        agentPath <- c.get[AgentPath]("agentPath")
        directors <- c.get[Option[SubagentId]]("subagentId").flatMap:
          case None => c.get[Option[Seq[SubagentId]]]("directors").map(_.toVector.flatten)
          case Some(subagentId) => Right(Seq(subagentId))
        agentRunId <- c.get[AgentRunId]("agentRunId")
        controllerId <- c.get[ControllerId]("controllerId")
        controllerRunId <- c.get[Option[ControllerRunId]]("controllerRunId")
      yield
        AgentDedicated(agentPath, directors, agentRunId, controllerId, controllerRunId)

  /** Agent is up and running. */
  final case class AgentReady(
    timezone: String,
    totalRunningTime: FiniteDuration,
    platformInfo: Option/*COMPATIBLE with v2.3*/[PlatformInfo])
  extends AgentEvent

  type AgentShutDown = AgentShutDown.type
  case object AgentShutDown extends AgentEvent

  implicit val jsonCodec: TypedJsonCodec[AgentEvent] = TypedJsonCodec(
    Subtype[AgentDedicated](aliases = Seq("AgentCreated")),
    Subtype(deriveCodec[AgentReady]),
    Subtype(AgentShutDown))

  intelliJuseImport(FiniteDurationJsonEncoder)
