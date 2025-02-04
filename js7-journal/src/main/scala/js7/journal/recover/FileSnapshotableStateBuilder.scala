package js7.journal.recover

import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{itemsPerSecondString, perSecondStringOnly}
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.ByteUnits
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.Nulls.nonNull
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichString}
import js7.data.cluster.ClusterState
import js7.data.event.JournalSeparators.{Commit, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournalState, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.recover.FileSnapshotableStateBuilder.*
import js7.journal.recover.JournalProgress.{AfterHeader, AfterSnapshotSection, InCommittedEventsSection, InSnapshotSection, InTransaction, Initial}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Deadline.now
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class FileSnapshotableStateBuilder[S <: SnapshotableState[S]](
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId])
  (using S: SnapshotableState.Companion[S]):

  private val since = now
  private val builder = S.newBuilder()
  private var _progress: JournalProgress = JournalProgress.Initial
  private var _state: S = null.asInstanceOf[S]
  private var _eventId: EventId = -999
  private var _eventCount = 0L

  private object transaction:
    var buffer: ArrayBuffer[Stamped[KeyedEvent[Event]]] | Null = null

    def begin(): Unit =
      require(!isInTransaction)
      buffer = new mutable.ArrayBuffer[Stamped[KeyedEvent[Event]]]

    def add(stamped: Stamped[KeyedEvent[Event]]): Unit =
      require(isInTransaction)
      buffer = buffer.nn :+ stamped

    def clear(): Unit =
      buffer = null

    private def isInTransaction = buffer != null

  def startWithState(
    journalHeader: JournalHeader,
    eventId: EventId,
    totalEventCount: Long,
    state: S)
  : Unit =
    this._progress = InCommittedEventsSection
    builder.initializeState(Some(journalHeader), eventId, state)
    _state = state
    _eventId = eventId
    _eventCount = totalEventCount - journalHeader.totalEventCount

  def put(journalRecord: Any): Unit =
    _progress match
      case Initial =>
        journalRecord match
          case journalHeader: JournalHeader =>
            logger.debug(journalHeader.toString)
            JournalHeader.checkedHeader[S](journalHeader, journalFileForInfo, expectedJournalId)
              .orThrow
            builder.addSnapshotObject(journalHeader)
            _progress = AfterHeader

          case _ => throw new IllegalArgumentException(
            s"Not a valid JS7 journal file: $journalFileForInfo. Expected a JournalHeader" +
              s" instead of ${journalRecord.toString.truncateWithEllipsis(100)}:")

      case AfterHeader =>
        if journalRecord != SnapshotHeader then throw new IllegalArgumentException(
          "Missing SnapshotHeader in journal file")
        _progress = InSnapshotSection

      case InSnapshotSection =>
        journalRecord match
          case SnapshotFooter =>
            builder.onAllSnapshotObjectsAdded()
            _progress = AfterSnapshotSection
          case _ =>
            builder.addSnapshotObject(journalRecord)

      case AfterSnapshotSection =>
        if journalRecord != EventHeader then throw new IllegalArgumentException(
          "Missing EventHeader in journal file")
        _state = builder.result()
        _eventId = _state.eventId
        //builder = null.asInstanceOf[SnapshotableStateBuilder[S]]
        _progress = InCommittedEventsSection

      case InCommittedEventsSection =>
        journalRecord match
          case Transaction =>
            transaction.begin()
            _progress = InTransaction
          case _ =>
            val stamped = cast[Stamped[KeyedEvent[Event]]](journalRecord)
            _state = _state.applyKeyedEvent(stamped.value).orThrow
            _eventId = stamped.eventId
            _eventCount += 1

      case InTransaction =>
        journalRecord match
          case Commit =>
            _progress = InCommittedEventsSection
            val stampedEvents = transaction.buffer.nn
            _state = _state.applyStampedEvents(stampedEvents).orThrow
            _eventId = _state.eventId
            _eventCount += stampedEvents.size
            transaction.clear()
          case _ =>
            transaction.add(cast[Stamped[KeyedEvent[Event]]](journalRecord))

  def rollbackToEventSection(): Unit =
    _progress match
      case InCommittedEventsSection =>
      case InTransaction =>
        rollback()
      case _ =>
        throw new IllegalStateException(s"rollbackToEventSection() but progress=${_progress}")

  private def rollback(): Unit =
    transaction.clear()
    _progress = InCommittedEventsSection

  def journalProgress: JournalProgress =
    _progress

  def fileJournalHeader: Option[JournalHeader] =
    builder.fileJournalHeader

  /** Calculated next JournalHeader. */
  def nextJournalHeader: Option[JournalHeader] =
    builder.fileJournalHeader.map(_.copy(
      eventId = _eventId,
      totalEventCount = totalEventCount,
      totalRunningTime = builder.fileJournalHeader.fold(ZeroDuration): header =>
        val lastJournalDuration = lastEventIdTimestamp - header.timestamp
        (header.totalRunningTime + lastJournalDuration).roundUpToNext(1.ms),
      timestamp = lastEventIdTimestamp))

  def totalEventCount: Long =
    builder.fileJournalHeader.fold(0L)(_.totalEventCount) + _eventCount

  private def lastEventIdTimestamp: Timestamp =
    if eventId == EventId.BeforeFirst then
      Timestamp.now
    else
      EventId.toTimestamp(eventId)

  def eventId: EventId =
    if nonNull(_state) then
      _eventId
    else
      builder.eventId

  def journalState: JournalState =
    if nonNull(_state) then
      _state.journalState
    else
      builder.journalState

  def clusterState: ClusterState =
    if nonNull(_state) then
      _state.clusterState
    else
      builder.clusterState

  def maybeState(): Option[S] =
    Option(_state)

  def result(): S =
    _state.withEventId(eventId = _eventId)

  def logStatistics(): Unit =
    val byteCount = Try(Files.size(journalFileForInfo)).toOption
    val elapsed = since.elapsed
    val snapshotCount = builder.snapshotCount
    if elapsed >= 1.s then
      logger.debug:
        itemsPerSecondString(elapsed, snapshotCount + _eventCount, "snapshots+events") +
          byteCount.fold("")(byteCount =>
            ", " + perSecondStringOnly(elapsed, byteCount / 1_000_000, "MB", gap = false) +
              " " + toKBGB(byteCount)
          ) + " read"
    if snapshotCount + _eventCount > 0 then
      val age = (Timestamp.now - EventId.toTimestamp(eventId)).withMillis(0).pretty
      logger.info(s"Recovered last EventId is ${EventId.toString(eventId)}, emitted $age ago " +
        s"($snapshotCount snapshot objects and ${_eventCount} events" +
        byteCount.fold("")(o => ", " + toKBGB(o)) +
        " read" +
        ((elapsed >= 10.s) ?? s" in ${elapsed.pretty}") +
        ")")


object FileSnapshotableStateBuilder:
  private val logger = Logger[this.type]
