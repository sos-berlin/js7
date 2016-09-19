package com.sos.scheduler.engine.data

import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.scheduler.engine.data.event.{Event, KeyedEvent, KeyedTypedEventJsonFormat}
import com.sos.scheduler.engine.data.filebased.FileBasedEvent
import com.sos.scheduler.engine.data.log.Logged
import com.sos.scheduler.engine.data.order.{OrderEvent, OrderStatisticsChanged}

/**
  * @author Joacim Zschimmer
  */
package object events {

  implicit val SchedulerAnyKeyedEventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[FileBasedEvent],
      KeyedSubtype[Logged],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[OrderStatisticsChanged])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit def schedulerKeyedEventJsonFormat[E <: Event]: KeyedTypedEventJsonFormat[E] =
    SchedulerAnyKeyedEventJsonFormat.asInstanceOf[KeyedTypedEventJsonFormat[E]]
}
