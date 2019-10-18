package com.sos.jobscheduler.agent.data.event

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.workflow.WorkflowEvent

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
      KeyedSubtype[AgentMasterEvent])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    AgentKeyedEventJsonCodec.asInstanceOf[KeyedEventTypedJsonCodec[Event]]
}
