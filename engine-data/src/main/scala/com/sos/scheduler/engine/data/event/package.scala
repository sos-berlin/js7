package com.sos.scheduler.engine.data

import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
package object event {

  type EventId = Long

  object EventId {
    val BeforeFirst: EventId = 0
    val IdsPerMillisecond = 1000

    def apply(eventId: String) = eventId.toLong

    def eventIdToInstant(id: EventId) = Instant ofEpochMilli id / 1000 plusNanos id % 1000 * 1000
  }
}
