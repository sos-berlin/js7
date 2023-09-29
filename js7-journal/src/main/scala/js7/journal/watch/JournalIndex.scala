package js7.journal.watch

import java.util.Arrays.binarySearch
import js7.base.log.Logger
import js7.base.utils.ByteUnits.toKBGB
import js7.common.jsonseq.PositionAnd
import js7.common.scalautil.Synchronizer
import js7.data.event.EventId
import js7.journal.watch.JournalIndex.*
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
private[watch] final class JournalIndex(torn: PositionAnd[EventId], size: Int):
  private var positions = new Array[Long]((size + 1) / 2 * 2)
  private var eventIds = new Array[Long](positions.length)
  private var length = 0
  private var _highestEventId = EventId.BeforeFirst - 1
  private var freezed = false
  private var spread = 1
  private var addedCount = 0

  require(positions.nonEmpty)

  addAfter(torn.value, torn.position)
  logger.debug(s"Building JournalIndex(${EventId.toString(torn.value)})")

  def addAfter(eventId: EventId, position: Long, n: Int = 1): Unit =
    if !tryAddAfter(eventId, position, n) then
      if eventId == _highestEventId then
        throw new IllegalArgumentException(s"JournalIndex: Duplicate EventId added: ${EventId.toString(eventId)}")
      else
        throw new IllegalArgumentException(s"JournalIndex: EventIds are in wrong order: ${EventId.toString(eventId)} ≥ ${EventId.toString(_highestEventId)}")

  def tryAddAfter(eventId: EventId, position: Long, n: Int = 1): Boolean =
    require(n > 0, "JournalIndex.tryAddAfter")
    eventId > _highestEventId && {
      synchronized:
        eventId > _highestEventId && {
          if freezed then throw new IllegalStateException(s"JournalIndex: tryAddAfter($eventId) after freeze ${_highestEventId} ?") // Self-check
          _highestEventId = eventId
          val a = addedCount
          addedCount += n
          if addedCount / spread > a / spread then
            if length == positions.length then
              compress(factor = 2)
            positions(length) = position
            eventIds(length) = eventId
            length += 1
          true
        }
    }

  /** toFactor > 1 to keep multiple JournalIndex small. */
  def freeze(toFactor: Int) =
    if !freezed then synchronized:
      if !freezed then
        val a = toFactor / spread min length / MinimumLength
        if a > 1 then compress(a)
        if length < positions.length then
          positions = shrinkArray(positions, length)
          eventIds = shrinkArray(eventIds, length)
        freezed = true
        logger.debug(s"Freezed $toString, memory=${toKBGB(positions.length * 2 * 8)}")

  @TestOnly
  def highestEventId = _highestEventId

  private def compress(factor: Int): Unit =
    for i <- 1 until length / factor do
      positions(i) = positions(factor * i)
      eventIds(i) = eventIds(factor * i)
    length = length / factor max 1
    spread = factor * spread
    logger.debug(s"Compressed $toString")

  private val buildSynchronizer = new Synchronizer("building JournalIndex")

  def synchronizeBuilding[A](body: => A): A =
    buildSynchronizer.synchronize:
      body

  def positionAfter(after: EventId): Long =
    positionAndEventIdAfter(after).position

  def positionAndEventIdAfter(after: EventId): PositionAnd[EventId] =
    synchronized:
      if length == 0 then throw new IllegalStateException("JournalIndex.positionAfter but length=0")
      val index = binarySearch(eventIds, 0, length, after) match
        case i if i >= 0 =>
          i

        case i =>
          if after < eventIds.head then throw new IllegalArgumentException(s"JournalIndex.positionAfter($after) but oldest EventId is ${eventIds.head}")
          -i - 2
      PositionAnd(positions(index), eventIds(index))

  @TestOnly
  def positionAndEventIds: Seq[PositionAnd[EventId]] =
    synchronized:
      for i <- 0 until length yield PositionAnd(positions(i), eventIds(i))

  override def toString =
    val addedFileSize = synchronized(if length == 0 then 0 else positions(length - 1) - torn.position)
    "JournalIndex(" +
      EventId.toString(torn.value) + ".." + EventId.toString(_highestEventId) +
      s" eventIds=$addedCount ${toKBGB(addedFileSize)}" +
      s" entries=$length/${eventIds.length} spread=$spread ⌀${toKBGB(addedFileSize / length)}"  +
      ")"

  private[watch] def spreadForTest = spread
  private[watch] def lengthForTest = length

object JournalIndex:
  private val MinimumLength = 100
  private val logger = Logger[this.type]

  private def shrinkArray(array: Array[Long], length: Int): Array[Long] =
    if length == array.length then
      array
    else
      val result = new Array[Long](length)
      Array.copy(array, 0, result, 0, length)
      result
