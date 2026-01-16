package js7.journal.recover

import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.PositionAnd
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.journal.recover.TransactionReader.*
import scala.collection.mutable.ArrayBuffer
import scala.language.unsafeNulls

private final class TransactionReader:
  private val buffer = new ArrayBuffer[PositionAnd[Stamped[KeyedEvent[Event]]]](InitialBufferSize)
  private var next = 0
  private var _inTransaction = false

  def begin(): Unit =
    assertThat(!isInTransaction)

  def add(positionAndStamped: PositionAnd[Stamped[KeyedEvent[Event]]]): Unit =
    _inTransaction = true
    buffer += positionAndStamped

  def onCommit(): Unit =
    next = 0

  def clear(): Unit =
    if buffer.size < KeepBufferSize then
      buffer.clear()
    else
      buffer.clearAndShrink(KeepBufferSize)
    _inTransaction = false

  def readNext(): Option[Stamped[KeyedEvent[Event]]] =
    isInTransaction ? {
      val stamped = buffer(next).value
      if next > 1 then buffer(next - 1) = null // Keep last event for positionAndEventId, free older entry
      next += 1
      if next == buffer.length then
        clear()
      stamped
    }

  /** May be called concurrently. */
  def positionAndEventId: Option[PositionAnd[EventId]] =
    (_inTransaction && next >= 1) ?
      PositionAnd(buffer(next).position, buffer(next - 1).value.eventId)

  // Do not use concurrently!
  def isInTransaction =
    _inTransaction

  def length =
    buffer.length


object TransactionReader:
  private val InitialBufferSize = 64
  private val KeepBufferSize = 16 * 1024 // TODO How big can multiple transactions get?
