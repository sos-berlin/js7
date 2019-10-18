package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.data.agent.AgentEventIdEvent

/**
  * @author Joacim Zschimmer
  */
package object events
{
  implicit val MasterKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[MasterEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[MasterAgentEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[AgentEventIdEvent])
}
