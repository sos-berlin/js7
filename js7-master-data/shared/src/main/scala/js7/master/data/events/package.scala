package js7.master.data

import js7.data.cluster.ClusterEvent
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}
import js7.data.filebased.RepoEvent
import js7.data.master.MasterFileBaseds._
import js7.data.order.OrderEvent
import js7.master.data.agent.AgentEventIdEvent

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
