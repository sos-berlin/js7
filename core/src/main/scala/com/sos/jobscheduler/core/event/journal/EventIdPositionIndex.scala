package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.data.event.EventId
import java.util.Arrays.binarySearch
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[journal] final class EventIdPositionIndex(size: Int)
{
  private val positions = new Array[Long]((size + 1) / 2 * 2)
  private val eventIds = new Array[Long](positions.length)
  private var length = 0
  private var highestEventId = EventId.BeforeFirst - 1

  require(positions.nonEmpty)

  def addAfter(eventId: EventId, position: Long): Unit =
    synchronized {
      if (eventId <= highestEventId)
        throw new IllegalArgumentException(s"EventIdPositionIndex: EventId out of order: ${EventId.toString(eventId)} > ${EventId.toString(highestEventId)}")
      highestEventId = eventId
      if (length == positions.length) {
        halve()
      }
      positions(length) = position
      eventIds(length) = eventId
      length += 1
    }

  private def halve(): Unit = {
    for (i ← 1 until length / 2) {
      positions(i) = positions(2 * i)
      eventIds(i) = eventIds(2 * i)
    }
    length = length / 2
  }

  def positionAfter(eventId: EventId): Long =
    synchronized {
      if (length == 0) throw new IllegalStateException("EventIdPositionIndex.positionAfter but length=0")
      binarySearch(eventIds, 0, length, eventId) match {
        case i if i >= 0 ⇒
          positions(i)

        case i ⇒
          if (eventId < eventIds.head) throw new IllegalArgumentException(s"EventIdPositionIndex.positionAfter($eventId) but oldest EventId is ${eventIds.head}")
          positions(-i - 2)
      }
    }

  @TestOnly
  private[journal] def positionAndEventIds: Seq[PositionAnd[EventId]] =
    synchronized {
      for (i ← 0 until length) yield PositionAnd(positions(i), eventIds(i))
    }
}
