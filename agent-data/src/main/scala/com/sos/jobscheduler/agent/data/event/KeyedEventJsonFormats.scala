package com.sos.jobscheduler.agent.data.event

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
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
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[WorkflowEvent.WorkflowAttached])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit def keyedEventJsonCodec[E <: Event]: KeyedEventTypedJsonCodec[E] =
    AgentKeyedEventJsonCodec.asInstanceOf[KeyedEventTypedJsonCodec[E]]
}
