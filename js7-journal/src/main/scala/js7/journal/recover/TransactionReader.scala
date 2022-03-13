package js7.journal.recover

import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.common.jsonseq.PositionAnd
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private final class TransactionReader
{
  private var buffer: ArrayBuffer[PositionAnd[Stamped[KeyedEvent[Event]]]] = null
  private var next = 0

  def begin(): Unit = {
    assertThat(!isInTransaction)
    buffer = new mutable.ArrayBuffer[PositionAnd[Stamped[KeyedEvent[Event]]]]
  }

  def add(positionAndStamped: PositionAnd[Stamped[KeyedEvent[Event]]]): Unit = {
    assertThat(isInTransaction)
    buffer += positionAndStamped
  }

  def onCommit(): Unit = {
    assertThat(isInTransaction)
    next = 0
    if (buffer.isEmpty) buffer = null
  }

  def clear(): Unit =
    buffer = null

  def readNext(): Option[Stamped[KeyedEvent[Event]]] =
    isInTransaction ? {
      val stamped = buffer(next).value
      if (next > 1) buffer(next - 1) = null  // Keep last event for positionAndEventId, free older entry
      next += 1
      if (next == buffer.length) {
        buffer = null
      }
      stamped
    }

  /** May be called concurrently. */
  def positionAndEventId: Option[PositionAnd[EventId]] =
    (buffer != null && next >= 1) ?
      PositionAnd(buffer(next).position, buffer(next - 1).value.eventId)

  // Do not use concurrently!
  def isInTransaction =
    buffer != null

  def length =
    buffer.length
}
