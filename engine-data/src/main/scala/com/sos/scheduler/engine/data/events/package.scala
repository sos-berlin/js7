package com.sos.scheduler.engine.data

import com.sos.scheduler.engine.data.event.KeyedEvent.{KeyedSubtype, KeyedTypedEventJsonFormat}
import com.sos.scheduler.engine.data.event.{Event, KeyedEvent}
import com.sos.scheduler.engine.data.filebased.FileBasedEvent
import com.sos.scheduler.engine.data.log.Logged
import com.sos.scheduler.engine.data.order.OrderEvent

/**
  * @author Joacim Zschimmer
  */
package object events {

  implicit val SchedulerKeyedEventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[FileBasedEvent],
      KeyedSubtype[Logged],
      KeyedSubtype[OrderEvent])
}
