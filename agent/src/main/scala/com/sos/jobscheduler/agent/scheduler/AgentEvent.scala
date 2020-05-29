package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.CirceUtils
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.zoneIdJsonEncoder
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec, NoKeyEvent}
import com.sos.jobscheduler.data.master.MasterId

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
