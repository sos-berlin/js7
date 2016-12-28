package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Snapshot}

/**
  * @author Joacim Zschimmer
  */
trait ForeignEventIdAdapting {
  this: EventCollector ⇒

  protected val eventIdGenerator: EventIdGenerator

  final def putForeignEventSnapshot(snapshot: Snapshot[AnyKeyedEvent]): Unit = {
    putEventSnapshot(adapt(snapshot))
  }

  private def adapt(snapshot: Snapshot[AnyKeyedEvent]): Snapshot[AnyKeyedEvent] = {
    if (lastEventId < snapshot.eventId)
      snapshot
    else
      Snapshot(lastEventId + 1, snapshot.value)
  }
}
