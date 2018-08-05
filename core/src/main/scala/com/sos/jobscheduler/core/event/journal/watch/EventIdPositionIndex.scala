package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
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
  private var freezed = false
  private var _factor = 1
  private var addedCount = 0

  require(positions.nonEmpty)

  def addAfter(eventId: EventId, position: Long, n: Int = 1): Unit =
    if (!tryAddAfter(eventId, position, n))
      throw new IllegalArgumentException(s"EventIdPositionIndex: EventId out of order: ${EventId.toString(eventId)} > ${EventId.toString(_highestEventId)}")

  def tryAddAfter(eventId: EventId, position: Long, n: Int = 1): Boolean =
    (eventId > _highestEventId) &&
      synchronized {
        if (freezed) throw new IllegalStateException("EventIdPositionIndex: tryAddAfter after freeze?")  // Self-check
        (eventId > _highestEventId) && {
          val a = addedCount
          addedCount += n
          if (addedCount / _factor > a / _factor) {
            _highestEventId = eventId
            if (length == positions.length) {
              compress(factor = 2)
            }
            positions(length) = position
            eventIds(length) = eventId
            length += 1
          }
          true
        }
      }

  def freeze(toFactor: Int) = {
    synchronized {
      val a = toFactor / _factor min length / MinimumLength
      if (a > 1) compress(a)
      if (length < positions.length) {
        positions = shrinkArray(positions, length)
        eventIds = shrinkArray(eventIds, length)
      }
      freezed = true
      logger.trace(s"Freezed - size=${positions.length} ${toKBGB(positions.length * 2 * 8)}")
    }
  }

  private def compress(factor: Int): Unit = {
    for (i ← 1 until length / factor) {
      positions(i) = positions(factor * i)
      eventIds(i) = eventIds(factor * i)
    }
    length = length / factor max 1
    _factor = factor * _factor
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

  private[watch] def factorForTest = _factor
  private[watch] def lengthForTest = length
}

object EventIdPositionIndex
{
  private val MinimumLength = 100
  private val logger = Logger(getClass)

  private def shrinkArray(array: Array[Long], length: Int): Array[Long] =
    if (length == array.length)
      array
    else {
      val result = new Array[Long](length)
      Array.copy(array, 0, result, 0, length)
      result
    }
}
