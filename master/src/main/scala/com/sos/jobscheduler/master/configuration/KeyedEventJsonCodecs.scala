package com.sos.jobscheduler.master.configuration

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.scheduledorder.OrderScheduleEvent

/**
  * @author Joacim Zschimmer
  */
private[master] object KeyedEventJsonCodecs
{
  /**
    * All publicly known event classes.
    */
  implicit val MasterJournalKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    MasterKeyedEventJsonCodec |
      KeyedEventTypedJsonCodec[Event](
        KeyedSubtype.singleEvent[AgentEventIdEvent],
        KeyedSubtype[OrderScheduleEvent])
}
