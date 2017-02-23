package com.sos.jobscheduler.master

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
  implicit val MasterKeyedEventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[OrderEvent],
      KeyedSubtype[JobnetEvent],
      KeyedSubtype[AgentEventIdEvent])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit def keyedEventJsonFormat[E <: Event]: KeyedTypedEventJsonFormat[E] =
    MasterKeyedEventJsonFormat.asInstanceOf[KeyedTypedEventJsonFormat[E]]
}
