package com.sos.jobscheduler.common.event.collector

import com.google.common.collect.{AbstractIterator ⇒ GuavaIterator}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, EventId, Stamped}
import java.util.NoSuchElementException
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
final class MemoryKeyedEventQueue(sizeLimit: Int)
{
  private val queue = new java.util.concurrent.ConcurrentSkipListMap[java.lang.Long, Stamped[AnyKeyedEvent]]
  private var queueLength: Int = 0
  @volatile
  private var lastRemovedFirstId: EventId = EventId.BeforeFirst

  def add(stamped: Stamped[AnyKeyedEvent]): Unit =
    synchronized {
      require(lastEventId < stamped.eventId,
        s"EventId '${EventId.toString(stamped.eventId)}' is not greater than last Eventid ${EventId.toString(lastEventId)}")
      if (queueLength >= sizeLimit) {
        lastRemovedFirstId = queue.firstKey
        queue.remove(lastRemovedFirstId)
        queueLength -= 1
      }
      queue.put(stamped.eventId, stamped)
      queueLength += 1
    }

  def hasAfter(after: EventId) = queue.navigableKeySet.higher(after) != null

  /**
    * Returns an Some[Iterator] of events or None if the first requested EventId has gone by.
    * The Iterator ends with the end of the queue when the queue has torn, if at least one event has been returned.
    * <p>
    *   None is returned if the queue has torn at its front (`after` < `tornEventId`).
    */
  def after(after: EventId): Option[Iterator[Stamped[AnyKeyedEvent]]] =
    synchronized {
      val result = new EventIterator(after, queue.navigableKeySet.tailSet(after, true).iterator.asScala map queue.get)
      val afterIsKnown = after == tornEventId || result.hasNext && result.next().eventId == after
      if (!afterIsKnown) None
      else
        try {
          // hasNext and peek may throw NoSuchElementException in case of race condition ?
          if (result.hasNext) result.peek
          // result is empty or has the first Stamped in its head buffer.
          after >= tornEventId option result.asScala
        } catch { case _: NoSuchElementException ⇒
          None
        }
    }

  def reverseEvents(after: EventId): Iterator[Stamped[AnyKeyedEvent]] =
    new EventIterator(EventId.MaxValue, queue.navigableKeySet.descendingIterator.asScala takeWhile { _ > after } map queue.get)
      .asScala

  def lastEventId: EventId =
    if (queue.isEmpty) tornEventId else queue.lastKey

  /** Events until this EventId are lost. */
  def tornEventId: EventId =
    lastRemovedFirstId

  private class EventIterator(firstEventId: EventId, stampedIterator: Iterator[Stamped[AnyKeyedEvent]])
  extends GuavaIterator[Stamped[AnyKeyedEvent]] {
    private var lastReturnedEventId = firstEventId

    def computeNext() =
      try
        if (stampedIterator.hasNext) {
          val stamped = stampedIterator.next()  // May throw NoSuchElementException in case of race condition ?
          if (lastReturnedEventId >= tornEventId && stamped != null) {
            lastReturnedEventId = stamped.eventId
            stamped
          } else
            endOfData
        } else
          endOfData
      catch { case _: NoSuchElementException ⇒
        endOfData
      }
  }

  override def toString = s"MemoryKeyedEventQueue(length=$queueLength, lastRemovedFirstId=$lastRemovedFirstId)"
}
