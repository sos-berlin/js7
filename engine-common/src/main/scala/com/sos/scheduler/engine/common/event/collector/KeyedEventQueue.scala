package com.sos.scheduler.engine.common.event.collector

import com.google.common.collect.{AbstractIterator ⇒ GuavaIterator}
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, EventId, Snapshot}
import java.util.NoSuchElementException
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventQueue(sizeLimit: Int) {
  private val queue = new java.util.concurrent.ConcurrentSkipListMap[java.lang.Long, Snapshot[AnyKeyedEvent]]
  private var queueSize: Int = 0
  @volatile
  private var lastRemovedFirstId: EventId = EventId.BeforeFirst

  /** Events before this EventId are lost. */
  def oldestEventId = lastRemovedFirstId

  def add(snapshot: Snapshot[AnyKeyedEvent]): Unit = {
    synchronized {
      require(lastEventId < snapshot.eventId,
        s"EventId '${EventId.toString(snapshot.eventId)}' is not greater than last Eventid ${EventId.toString(lastEventId)}")
      if (queueSize >= sizeLimit) {
        lastRemovedFirstId = queue.firstKey
        queue.remove(lastRemovedFirstId)
        queueSize -= 1
      }
      queue.put(snapshot.eventId, snapshot)
      queueSize += 1
    }
  }

  def hasAfter(after: EventId) = queue.navigableKeySet.higher(after) != null

  /**
    * Returns an Some[Iterator] of events or None if the first requested EventId has gone by.
    * The Iterator ends with the end of the queue when the queue has torn, if at least one event has been returned.
    * <p>
    *   None is returned if the queue has torn at its front (`after` < `oldestEventId`).
    */
  def after(after: EventId): Option[Iterator[Snapshot[AnyKeyedEvent]]] = {
    val result = new EventIterator(after, queue.navigableKeySet.tailSet(after, false).iterator map queue.get)
    try {
      // hasNext and peek may throw NoSuchElementException in case of race condition ?
      if (result.hasNext) result.peek
      // result is empty or has the first Snapshot in its head buffer.
      after >= oldestEventId option result
    } catch { case _: NoSuchElementException ⇒
      None
    }
  }

  def reverseEvents(after: EventId): Iterator[Snapshot[AnyKeyedEvent]] =
    new EventIterator(EventId.MaxValue, queue.navigableKeySet.descendingIterator takeWhile { _ > after } map queue.get)

  def lastEventId: EventId =
    if (queue.isEmpty) EventId.BeforeFirst else queue.lastKey

  def size: Int =
    queueSize

  private class EventIterator(firstEventId: EventId, snapshots: Iterator[Snapshot[AnyKeyedEvent]])
  extends GuavaIterator[Snapshot[AnyKeyedEvent]] {
    private var lastReturnedEventId = firstEventId

    def computeNext() =
      try
        if (snapshots.hasNext) {
          val snapshot = snapshots.next()  // May throw NoSuchElementException in case of race condition ?
          if (lastReturnedEventId >= oldestEventId && snapshot != null) {
            lastReturnedEventId = snapshot.eventId
            snapshot
          } else
            endOfData
        } else
          endOfData
      catch { case _: NoSuchElementException ⇒
        endOfData
      }
  }
}
