package js7.agent.scheduler

import js7.base.circeutils.CirceUtils
import js7.base.circeutils.JavaJsonCodecs.zoneIdJsonEncoder
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec, NoKeyEvent}
import js7.data.master.MasterId

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentEvent extends NoKeyEvent

object AgentEvent
{
  intelliJuseImport(zoneIdJsonEncoder)

  sealed trait AgentMasterEvent extends AgentEvent

  final case class MasterRegistered(
    masterId: MasterId,
    agentRefPath: AgentRefPath,
    agentRunId: AgentRunId)
  extends AgentMasterEvent

  implicit val jsonCodec: TypedJsonCodec[AgentEvent] = TypedJsonCodec[AgentEvent](
    Subtype(CirceUtils.deriveCodec[MasterRegistered])
  )
  implicit val KeyedEventJsonCodec = KeyedEventTypedJsonCodec[Event](
    KeyedSubtype[JournalEvent],
    KeyedSubtype[AgentEvent])
}
