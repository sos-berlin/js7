package com.sos.jobscheduler.master.configuration

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.master.data.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec

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
        KeyedSubtype.singleEvent[AgentEventIdEvent])
}
