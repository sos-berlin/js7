package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.order.OrderEvent

/**
  * @author Joacim Zschimmer
  */
package object events
{
  implicit val MasterKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[RepoEvent],
      KeyedSubtype[MasterEvent],
      KeyedSubtype[MasterAgentEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[AgentEventIdEvent])
}
