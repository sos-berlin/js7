package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Stamped}

/**
  * @author Joacim Zschimmer
  */
trait ForeignEventIdAdapting {
  this: EventCollector ⇒

  protected val eventIdGenerator: EventIdGenerator

  final def putForeignEventStamped(snapshot: Stamped[AnyKeyedEvent]): Unit = {
    addStamped(adapt(snapshot))
  }

  private def adapt(snapshot: Stamped[AnyKeyedEvent]): Stamped[AnyKeyedEvent] = {
    if (lastEventId < snapshot.eventId)
      snapshot
    else
      Stamped(lastEventId + 1, snapshot.value)
  }
}
