package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Stamped}

/**
  * @author Joacim Zschimmer
  */
trait ForeignEventIdAdapting {
  this: EventCollector ⇒

  protected val eventIdGenerator: EventIdGenerator

  final def putForeignEventStamped(stamped: Stamped[AnyKeyedEvent]): Unit = {
    addStamped(adapt(stamped))
  }

  private def adapt(stamped: Stamped[AnyKeyedEvent]): Stamped[AnyKeyedEvent] = {
    if (lastEventId < stamped.eventId)
      stamped
    else
      Stamped(lastEventId + 1, stamped.value)
  }
}
