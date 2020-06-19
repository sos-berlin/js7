package js7.agent.scheduler

import js7.base.circeutils.CirceUtils
import js7.base.circeutils.JavaJsonCodecs.zoneIdJsonEncoder
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec, NoKeyEvent}

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentEvent extends NoKeyEvent

object AgentEvent
{
  intelliJuseImport(zoneIdJsonEncoder)

  sealed trait AgentControllerEvent extends AgentEvent

  final case class ControllerRegistered(
    controllerId: ControllerId,
    agentRefPath: AgentRefPath,
    agentRunId: AgentRunId)
  extends AgentControllerEvent

  implicit val jsonCodec: TypedJsonCodec[AgentEvent] = TypedJsonCodec[AgentEvent](
    Subtype(CirceUtils.deriveCodec[ControllerRegistered])
  )
  implicit val KeyedEventJsonCodec = KeyedEventTypedJsonCodec[Event](
    KeyedSubtype[JournalEvent],
    KeyedSubtype[AgentEvent])
}
