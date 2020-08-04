package js7.controller.data

import js7.controller.data.agent.AgentEventIdEvent
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerItems._
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}
import js7.data.item.RepoEvent
import js7.data.order.OrderEvent

/**
  * @author Joacim Zschimmer
  */
package object events
{
  implicit val ControllerKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[ControllerAgentEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[AgentEventIdEvent])
}
