package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Snapshot}

/**
  * @author Joacim Zschimmer
  */
trait EventIdGenerating {
  this: EventCollector ⇒

  protected val eventIdGenerator: EventIdGenerator

  protected final def putEvent(keyedEvent: AnyKeyedEvent): Unit = {
    val eventId = eventIdGenerator.next()
    putEventSnapshot(Snapshot(eventId, keyedEvent))
  }
}
