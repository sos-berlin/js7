package js7.journal.recover

import cats.syntax.option.*
import io.circe.Json
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import js7.data.event.{Event, EventId, JournalSeparators, KeyedEvent, Stamped}
import js7.journal.recover.TransactionReader.*
import scala.collection.mutable.ArrayBuffer
import scala.language.unsafeNulls
import scala.util.control.NonFatal

private final class TransactionReader(
  jsonReader: InputStreamJsonSeqReader,
  deserialize: Json => Stamped[KeyedEvent[Event]],
  bufferSize: Int):

  private val buffer = new ArrayBuffer[PositionAnd[Stamped[KeyedEvent[Event]]]](InitialBufferSize)
  private var next = 0
  private var transactionLength = 0
  private var remainingTransactionPos = -1L
  private var inTransaction = false
  private var committed = false
  private var lastPositionAndEventId = none[PositionAnd[EventId]]

  def begin(): Unit =
    assertThat(!inTransaction)
    inTransaction = true

  def add(positionAndStamped: PositionAnd[Json]): Unit =
    try
      if transactionLength < bufferSize then
        buffer += positionAndStamped.copy(value = deserialize(positionAndStamped.value))
      else
        // We have to deserialize the event (and throw away the result), just because we want to
        // be sure, that the JSON object is deserializable.
        // Otherwise, a corrupt transaction could be read partially, breaking the atomicity.
        deserialize(positionAndStamped.value)
        if transactionLength == bufferSize then
          remainingTransactionPos = positionAndStamped.position
      transactionLength += 1
    catch case NonFatal(t) =>
      clear()
      throw t

  def onCommit(): Unit =
    assertThat(inTransaction)
    committed = true

  def readNext(): Option[Stamped[KeyedEvent[Event]]] =
    inTransaction thenMaybe:
      assertThat(committed)
      if transactionLength == 0 then
        clear()
        None
      else
        assertThat(next < transactionLength)
        val stamped =
          try
            if next < bufferSize then
              readBuffered()
            else
              readFromFile()
          catch case NonFatal(t) =>
            clear() // Free memory
            throw t
        next += 1
        if next == transactionLength then
          clear()
        Some(stamped)

  private def readBuffered(): Stamped[KeyedEvent[Event]] =
    val stamped = buffer(next).value
    if next < transactionLength - 1 then
      // The position of the next event, which is in the buffer or the first unbuffered event
      val nextPosition =
        if next + 1 < bufferSize then buffer(next + 1).position else remainingTransactionPos
      lastPositionAndEventId = Some(PositionAnd(nextPosition, stamped.eventId))
    buffer(next) = null // Release memory
    stamped

  private def readFromFile(): Stamped[KeyedEvent[Event]] =
    if next == bufferSize then
      jsonReader.seek(remainingTransactionPos)
    val stamped = deserialize(readRecord().value)
    if next == transactionLength - 1 then
      if readRecord().value != JournalSeparators.Commit then
        throw IllegalStateException("\"COMMIT\" record is missing")
    else
      lastPositionAndEventId = Some(PositionAnd(jsonReader.position, stamped.eventId))
    stamped

  private def readRecord(): PositionAnd[Json] =
    jsonReader.read().getOrElse:
      throw IllegalStateException("Transaction truncated while reading")

  def clear(): Unit =
    buffer.clear()
    next = 0
    transactionLength = 0
    remainingTransactionPos = -1
    inTransaction = false
    committed = false
    lastPositionAndEventId = None

  /** May be called concurrently. */
  def positionAndEventId: Option[PositionAnd[EventId]] =
    lastPositionAndEventId

  // Do not use concurrently!
  def isInTransaction: Boolean =
    inTransaction

  def length: Int =
    transactionLength


object TransactionReader:
  private val InitialBufferSize = 64
