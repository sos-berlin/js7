package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Stamped}

/**
  * @author Joacim Zschimmer
  */
trait EventIdGenerating {
  this: EventCollector =>

  protected val eventIdGenerator: EventIdGenerator

  protected final def putEvent(keyedEvent: AnyKeyedEvent): Unit = {
    val timestamp = Timestamp.now
    val eventId = eventIdGenerator.next()
    addStamped(Stamped(eventId, timestamp, keyedEvent))
  }
}
