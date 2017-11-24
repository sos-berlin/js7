package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, KeyedTypedEventJsonFormat}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.workflow.WorkflowEvent
import com.sos.jobscheduler.master.order.OrderScheduleEvent

/**
  * @author Joacim Zschimmer
  */
object KeyedEventJsonFormats {
  /**
    * All publicly known event classes.
    */
  implicit val MasterKeyedEventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[OrderEvent],
      KeyedSubtype[WorkflowEvent],
      KeyedSubtype[AgentEventIdEvent],
      KeyedSubtype[OrderScheduleEvent])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit def keyedEventJsonFormat[E <: Event]: KeyedTypedEventJsonFormat[E] =
    MasterKeyedEventJsonFormat.asInstanceOf[KeyedTypedEventJsonFormat[E]]
}
