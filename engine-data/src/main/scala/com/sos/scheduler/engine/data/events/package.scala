package com.sos.scheduler.engine.data

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.KeyedEvent.{KeyedSubtype, KeyedTypedEventJsonFormat}
import com.sos.scheduler.engine.data.event.{Event, KeyedEvent}
import com.sos.scheduler.engine.data.filebased.FileBasedEvent
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

  type AnyEvent = Event { type Key = Any }

  implicit val XXXEventJsonFormat = TypedJsonFormat[Event](
      Subtype[LogEvent],
      Subtype[FileBasedEvent],
      Subtype[OrderEvent])
  implicit val AnyEventJsonFormat = XXXEventJsonFormat.asInstanceOf[TypedJsonFormat[AnyEvent]]
}
