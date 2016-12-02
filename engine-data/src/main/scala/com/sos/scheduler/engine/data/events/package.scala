package com.sos.scheduler.engine.data

import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.scheduler.engine.data.event.{Event, KeyedEvent, KeyedTypedEventJsonFormat}
import com.sos.scheduler.engine.data.filebased.FileBasedEvent
import com.sos.scheduler.engine.data.job.{JobEvent, TaskEvent}
import com.sos.scheduler.engine.data.log.Logged
import com.sos.scheduler.engine.data.order.{JobChainEvent, JocOrderStatisticsChanged, OrderEvent}

/**
  * @author Joacim Zschimmer
  */
package object events {

  /**
    * All publicly known event classes.
    */
  implicit val SchedulerAnyKeyedEventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[FileBasedEvent],
      KeyedSubtype[JobChainEvent],
      KeyedSubtype[JobEvent],
      KeyedSubtype[JocOrderStatisticsChanged],
      KeyedSubtype[Logged],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[TaskEvent])

  /**
    * All subtypes of `Event` are serialized as `Event`.
    */
  implicit def schedulerKeyedEventJsonFormat[E <: Event]: KeyedTypedEventJsonFormat[E] =
    SchedulerAnyKeyedEventJsonFormat.asInstanceOf[KeyedTypedEventJsonFormat[E]]
}
