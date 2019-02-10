package com.sos.jobscheduler.master.configuration

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.sos.jobscheduler.master.scheduledorder.OrderScheduleEvent

/**
  * @author Joacim Zschimmer
  */
object KeyedEventJsonCodecs
{
  /**
    * All publicly known event classes.
    */
  implicit val MasterKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[MasterEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[MasterAgentEvent],
      KeyedSubtype.singleEvent[AgentEventIdEvent],
      KeyedSubtype[OrderScheduleEvent])
}
