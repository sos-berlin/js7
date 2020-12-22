package js7.journal.watch.collector

import js7.base.time.Timestamp
import js7.data.event.{AnyKeyedEvent, Stamped}
import js7.journal.EventIdGenerator

/**
  * @author Joacim Zschimmer
  */
private[collector] trait EventIdGenerating {
  this: EventCollector =>

  protected val eventIdGenerator: EventIdGenerator

  protected final def putEvent(keyedEvent: AnyKeyedEvent): Unit = {
    val timestamp = Timestamp.now
    val eventId = eventIdGenerator.next()
    addStamped(Stamped(eventId, timestamp, keyedEvent))
  }
}
