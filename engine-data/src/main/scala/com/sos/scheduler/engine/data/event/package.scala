package com.sos.scheduler.engine.data

import java.time.Instant
import spray.json.{JsNumber, JsValue}

/**
  * @author Joacim Zschimmer
  */
package object event {

  type EventId = Long

  object EventId {
    val BeforeFirst: EventId = 0
    val IdsPerMillisecond = 1000
    private[event] val JsonMaxValue = EventId(0x20000000000000L)  // =2^53 == 9007199254740992L

    def apply(eventId: String) = eventId.toLong

    def apply(eventId: Long) = eventId

    def toInstant(id: EventId) = Instant ofEpochMilli id / 1000 plusNanos id % 1000 * 1000

    def toJsValue(eventId: EventId) = JsNumber(eventId)

    def fromJsValue(o: JsValue) = o.asInstanceOf[JsNumber].value.toLongExact
  }

  type AnyKeyedEvent = KeyedEvent[Event]
  type AnyEvent = Event { type Key = Any }
}
