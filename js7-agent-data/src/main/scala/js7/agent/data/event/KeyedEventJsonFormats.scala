package js7.agent.data.event

import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}
import js7.data.order.OrderEvent
import js7.data.workflow.WorkflowEvent

/**
  * @author Joacim Zschimmer
  */
object KeyedEventJsonFormats
{
  /**
    * All publicly known event classes.
    */
  implicit val AgentKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[WorkflowEvent.WorkflowAttached],
      KeyedSubtype[AgentControllerEvent])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    AgentKeyedEventJsonCodec.asInstanceOf[KeyedEventTypedJsonCodec[Event]]
}
