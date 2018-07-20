package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.master.MasterId

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentEvent extends Event

object AgentEvent {

  sealed trait AgentMasterEvent extends AgentEvent {
    type Key = MasterId
  }

  final case object MasterAdded extends AgentMasterEvent

  implicit val KeyedEventJsonCodec = KeyedEventTypedJsonCodec[AgentEvent](
    KeyedSubtype(MasterAdded))
}
