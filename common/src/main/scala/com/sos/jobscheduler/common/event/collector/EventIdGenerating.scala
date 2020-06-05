package js7.common.event.collector

import js7.base.time.Timestamp
import js7.common.event.EventIdGenerator
import js7.data.event.{AnyKeyedEvent, Stamped}

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
