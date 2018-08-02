package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.watch.EventIdPositionIndex._
import com.sos.jobscheduler.data.event.EventId
import java.util.Arrays.binarySearch
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[watch] final class EventIdPositionIndex(size: Int)
{
  private var positions = new Array[Long]((size + 1) / 2 * 2)
  private var eventIds = new Array[Long](positions.length)
  private var length = 0
  private var _highestEventId = EventId.BeforeFirst - 1
  private var unusedMemoryReleased = false

  require(positions.nonEmpty)

  def addAfter(eventId: EventId, position: Long): Unit =
    if (!tryAddAfter(eventId, position))
      throw new IllegalArgumentException(s"EventIdPositionIndex: EventId out of order: ${EventId.toString(eventId)} > ${EventId.toString(_highestEventId)}")

  def tryAddAfter(eventId: EventId, position: Long): Boolean =
    (eventId > _highestEventId) &&
      synchronized {
        if (unusedMemoryReleased) throw new IllegalStateException("EventIdPositionIndex: tryAddAfter after releaseUnusedMemory?")  // Self-check
        (eventId > _highestEventId) && {
          _highestEventId = eventId
          if (length == positions.length) {
            halve()
          }
          positions(length) = position
          eventIds(length) = eventId
          length += 1
          true
        }
      }

  private def halve(): Unit = {
    for (i ← 1 until length / 2) {
      positions(i) = positions(2 * i)
      eventIds(i) = eventIds(2 * i)
    }
    length = length / 2
  }

  def releaseUnusedMemory() = {
    unusedMemoryReleased = true
    if (length < positions.length) {
      synchronized {
        positions = shrinkArray(positions, length)
        eventIds = shrinkArray(eventIds, length)
      }
    }
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
  def positionAndEventIds: Seq[PositionAnd[EventId]] =
    synchronized {
      for (i ← 0 until length) yield PositionAnd(positions(i), eventIds(i))
    }

  def highestEvenId = _highestEventId
}

object EventIdPositionIndex
{
  private def shrinkArray(array: Array[Long], length: Int): Array[Long] =
    if (length == array.length)
      array
    else {
      val result = new Array[Long](length)
      Array.copy(array, 0, result, 0, length)
      result
    }
}
