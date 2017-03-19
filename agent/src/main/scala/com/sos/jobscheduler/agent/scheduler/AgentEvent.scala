package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import spray.json.DefaultJsonProtocol.jsonFormat0

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentEvent extends Event

object AgentEvent {

  sealed trait MasterEvent extends AgentEvent {
    type Key = UserId
  }

  final case object MasterAdded extends MasterEvent {
    implicit val jsonFormat = jsonFormat0(() ⇒ MasterAdded)
  }

  implicit val KeyedEventJsonFormat = KeyedEvent.typedJsonFormat[AgentEvent](
    KeyedSubtype[MasterAdded.type])
}
