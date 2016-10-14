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
    // JavaScript uses floating point for all numbers, so it have 11 bits for a precise integer to
    // represent all integers between 0 and 2**53 (9.007.199.254.740.992).
    // 2 ** 53 = 9.007.199.254.740.992µs = 285 years. This is good until year 2255, for a million events/s.
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
