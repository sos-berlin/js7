package com.sos.scheduler.engine.data

import com.sos.scheduler.engine.data.event.KeyedEvent.{KeyedSubtype, KeyedTypedEventJsonFormat}
import com.sos.scheduler.engine.data.event.{KeyedEvent, Event}
import com.sos.scheduler.engine.data.log.LogEvent
import com.sos.scheduler.engine.data.order.OrderEvent

/**
  * @author Joacim Zschimmer
  */
package object events {

  implicit val EventJsonFormat: KeyedTypedEventJsonFormat[Event] =
    KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[LogEvent],
      KeyedSubtype[OrderEvent])
}
