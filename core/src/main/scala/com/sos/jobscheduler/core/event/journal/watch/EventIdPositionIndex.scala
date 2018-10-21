package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.watch.EventIdPositionIndex._
import com.sos.jobscheduler.data.event.EventId
import java.util.Arrays.binarySearch
import java.util.concurrent.TimeUnit.SECONDS
import java.util.concurrent.locks.ReentrantLock
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.blocking

/**
  * @author Joacim Zschimmer
  */
private[watch] final class EventIdPositionIndex(torn: PositionAnd[EventId], size: Int)
{
  private var positions = new Array[Long]((size + 1) / 2 * 2)
  private var eventIds = new Array[Long](positions.length)
  private var length = 0
  private var _highestEventId = EventId.BeforeFirst - 1
  private var freezed = false
  private var spread = 1
  private var addedCount = 0

  require(positions.nonEmpty)

  addAfter(torn.value, torn.position)
  logger.debug(s"Building EventIdPositionIndex(${EventId.toString(torn.value)})")

  def addAfter(eventId: EventId, position: Long, n: Int = 1): Unit =
    if (!tryAddAfter(eventId, position, n))
      throw new IllegalArgumentException(s"EventIdPositionIndex: EventId out of order: ${EventId.toString(eventId)} ≥ ${EventId.toString(_highestEventId)}")

  def tryAddAfter(eventId: EventId, position: Long, n: Int = 1): Boolean = {
    require(n >= 1, "EventIdPositionIndex.tryAddAfter")
    eventId > _highestEventId && {
      synchronized {
        eventId > _highestEventId && {
          if (freezed) throw new IllegalStateException("EventIdPositionIndex: tryAddAfter after freeze?")  // Self-check
          val a = addedCount
          addedCount += n
          if (addedCount / spread > a / spread) {
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
    }
  }

  /** toFactor > 1 to keep multiple EventIdPositionIndex small. */
  def freeze(toFactor: Int) =
    if (!freezed) synchronized {
      if (!freezed) {
        val a = toFactor / spread min length / MinimumLength
        if (a > 1) compress(a)
        if (length < positions.length) {
          positions = shrinkArray(positions, length)
          eventIds = shrinkArray(eventIds, length)
        }
        freezed = true
        logger.trace(s"Freezed - size=${positions.length} ${toKBGB(positions.length * 2 * 8)}")
      }
    }

  def isFreezed = freezed

  private def compress(factor: Int): Unit = {
    for (i ← 1 until length / factor) {
      positions(i) = positions(factor * i)
      eventIds(i) = eventIds(factor * i)
    }
    length = length / factor max 1
    spread = factor * spread
  }

  private val synchronizeLock = new ReentrantLock

  def synchronize[A](body: ⇒ A): A =
    blocking {
      try {
        if (!synchronizeLock.tryLock(1, SECONDS)) {
          try logger.debug(s"EventIdPositionIndex.synchronize: waiting for lock (#${synchronizeLock.getQueueLength})")
          catch { case _: Throwable ⇒ }  // No exceptions allowed here
          synchronizeLock.lock()
          try logger.debug(s"EventIdPositionIndex.synchronize: continuing")
          catch { case _: Throwable ⇒ }  // No exceptions allowed here
        }
        synchronized {
          body
        }
      } finally synchronizeLock.unlock()
    }

  def positionAfter(after: EventId): Long =
    positionAndEventIdAfter(after).position

  def positionAndEventIdAfter(after: EventId): PositionAnd[EventId] =
    synchronized {
      if (length == 0) throw new IllegalStateException("EventIdPositionIndex.positionAfter but length=0")
      val index = binarySearch(eventIds, 0, length, after) match {
        case i if i >= 0 ⇒
          i

        case i ⇒
          if (after < eventIds.head) throw new IllegalArgumentException(s"EventIdPositionIndex.positionAfter($after) but oldest EventId is ${eventIds.head}")
          -i - 2
      }
      PositionAnd(positions(index), eventIds(index))
    }

  @TestOnly
  def positionAndEventIds: Seq[PositionAnd[EventId]] =
    synchronized {
      for (i ← 0 until length) yield PositionAnd(positions(i), eventIds(i))
    }

  def highestEvenId = _highestEventId

  private[watch] def spreadForTest = spread
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
