package com.sos.jobscheduler.agent.scheduler.event

import com.sos.jobscheduler.data.engine2.order.{JobnetEvent, OrderEvent}
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, KeyedTypedEventJsonFormat}

/**
  * @author Joacim Zschimmer
  */
object KeyedEventJsonFormats {
  /**
    * All publicly known event classes.
    */
  implicit val AgentKeyedEventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[OrderEvent],
      KeyedSubtype[JobnetEvent])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit def keyedEventJsonFormat[E <: Event]: KeyedTypedEventJsonFormat[E] =
    AgentKeyedEventJsonFormat.asInstanceOf[KeyedTypedEventJsonFormat[E]]
}
