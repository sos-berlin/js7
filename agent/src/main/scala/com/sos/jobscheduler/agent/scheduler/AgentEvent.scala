package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentEvent extends Event

object AgentEvent {

  sealed trait MasterEvent extends AgentEvent {
    type Key = UserId
  }

  final case object MasterAdded extends MasterEvent

  implicit val KeyedEventJsonCodec = KeyedEventTypedJsonCodec[AgentEvent](
    KeyedSubtype(MasterAdded))
}
