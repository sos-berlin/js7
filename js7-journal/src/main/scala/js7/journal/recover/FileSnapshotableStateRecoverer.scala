package js7.journal.recover

import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{itemsPerSecondString, perSecondStringOnly}
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichString}
import js7.data.cluster.ClusterState
import js7.data.event.JournalSeparators.{Commit, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournalState, KeyedEvent, SnapshotableState, SnapshotableStateRecoverer, Stamped}
import js7.journal.recover.FileSnapshotableStateRecoverer.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Deadline.now
import scala.util.Try

final class FileSnapshotableStateRecoverer[S <: SnapshotableState[S]](
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId])
  (using S: SnapshotableState.Companion[S]):

  private val since = now
  private var _progress: Progress = Initial(S.newRecoverer())
  private var _snapshotCount = 0
  private var _eventCount = 0L

  def startWithState(
    journalHeader: JournalHeader,
    eventId: EventId,
    totalEventCount: Long,
    state: S)
  : Unit =
    _progress = InCommittedEventsSection(journalHeader, state, eventId)
    _eventCount = totalEventCount - journalHeader.totalEventCount

  def put(journalRecord: Any): Unit =
    _progress match
      case Initial(recoverer) =>
        journalRecord match
          case journalHeader: JournalHeader =>
            logger.debug(journalHeader.toString)
            JournalHeader.checkedHeader[S](journalHeader, journalFileForInfo, expectedJournalId)
              .orThrow
            recoverer.addSnapshotObject(journalHeader)
            _snapshotCount += 1
            _progress = AfterHeader(recoverer, journalHeader)

          case _ => throw new IllegalArgumentException(
            s"Not a valid JS7 journal file: $journalFileForInfo. Expected a JournalHeader" +
              s" instead of ${journalRecord.toString.truncateWithEllipsis(100)}:")

      case AfterHeader(recoverer, journalHeader) =>
        if journalRecord != SnapshotHeader then throw new IllegalArgumentException(
          "Missing SnapshotHeader in journal file")
        _progress = InSnapshotSection(recoverer, journalHeader)

      case InSnapshotSection(recoverer, journalHeader) =>
        journalRecord match
          case SnapshotFooter =>
            _progress = AfterSnapshotSection(journalHeader, recoverer.result())
            //recoverer = null.asInstanceOf[SnapshotableStateRecoverer[S]]
          case _ =>
            recoverer.addSnapshotObject(journalRecord)
            _snapshotCount += 1

      case AfterSnapshotSection(journalHeader, state) =>
        if journalRecord != EventHeader then throw new IllegalArgumentException(
          "Missing EventHeader in journal file")
        _progress = InCommittedEventsSection(journalHeader, state, state.eventId)

      case progress: InCommittedEventsSection =>
        journalRecord match
          case Transaction =>
            _progress = InTransaction(progress.journalHeader, progress.state)
          case _ =>
            val stamped = cast[Stamped[KeyedEvent[Event]]](journalRecord)
            progress.rawState = progress.rawState.applyKeyedEvent(stamped.value).orThrow
            progress.eventId = stamped.eventId
            _eventCount += 1

      case ta: InTransaction =>
        journalRecord match
          case Commit =>
            val events = ta.stampedKeyedEvents
            val updated = ta.state.applyStampedEvents(events).orThrow
            _progress = InCommittedEventsSection(ta.journalHeader, updated, updated.eventId)
            _eventCount += events.size
          case _ =>
            ta.add(cast[Stamped[KeyedEvent[Event]]](journalRecord))

  def rollbackToEventSection(): Unit =
    _progress match
      case _: InCommittedEventsSection =>
      case InTransaction(journalHeader, state) =>
        _progress = InCommittedEventsSection(journalHeader, state, state.eventId)
      case _ =>
        throw new IllegalStateException(s"rollbackToEventSection() but progress=${_progress}")

  def isAfterSnapshotSection: Boolean =
    _progress.isInstanceOf[AfterSnapshotSection]

  def isInCommittedEventsSection: Boolean =
    _progress.isInstanceOf[InCommittedEventsSection]

  def fileJournalHeader: Option[JournalHeader] =
    _progress match
      case o: HasJournalHeader => Some(o.journalHeader)
      case _ => None

  /** Calculated next JournalHeader. */
  def nextJournalHeader: Option[JournalHeader] =
    fileJournalHeader.map: header =>
      if _progress.eventId == header.eventId then sys.error:
        s"Journal file contains no event — it must start with SnapshotTaken: $journalFileForInfo"
      header.nextGeneration(
        eventId = _progress.eventId,
        totalEventCount = header.totalEventCount + _eventCount,
        totalRunningTime =
          val lastJournalDuration = lastEventIdTimestamp - header.timestamp
          (header.totalRunningTime + lastJournalDuration).roundUpToNext(1.ms),
        timestamp = lastEventIdTimestamp)

  def totalEventCount: Long =
    fileJournalHeader.fold(0L)(_.totalEventCount) + _eventCount

  private def lastEventIdTimestamp: Timestamp =
    if eventId == EventId.BeforeFirst then
      Timestamp.now
    else
      EventId.toTimestamp(eventId)

  def eventId: EventId =
    _progress.eventId

  def journalState: JournalState =
    _progress.journalState

  def clusterState: ClusterState =
    _progress.clusterState

  def result(): S =
    _progress match
      case o: HasState => o.state
      case _ => throw new IllegalStateException(
        s"$toString has not yet recovered all snapshot objects for $S")

  def logStatistics(): Unit =
    val byteCount = Try(Files.size(journalFileForInfo)).toOption
    val elapsed = since.elapsed
    val snapshotCount = _snapshotCount
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

  // Progress //

  private sealed trait Progress:
    def eventId: EventId
    def journalState: JournalState
    def clusterState: ClusterState

  private sealed trait HasRecoverer extends  Progress:
    def recoverer: SnapshotableStateRecoverer[S]
    final def eventId = recoverer.eventId
    final def journalState = recoverer.journalState
    final def clusterState = recoverer.clusterState

  private sealed trait HasJournalHeader extends Progress:
    def journalHeader: JournalHeader

  private sealed trait HasState extends HasJournalHeader:
    /** S without valid EventId. */
    def rawState: S
    def state: S
    final def journalState = state.journalState
    final def clusterState = state.clusterState


  private final case class Initial(recoverer: SnapshotableStateRecoverer[S])
  extends HasRecoverer

  private final case class AfterHeader(
    recoverer: SnapshotableStateRecoverer[S],
    journalHeader: JournalHeader)
  extends HasRecoverer

  private final case class InSnapshotSection(
    recoverer: SnapshotableStateRecoverer[S],
    journalHeader: JournalHeader)
  extends HasRecoverer

  private final case class AfterSnapshotSection(journalHeader: JournalHeader, state: S)
  extends HasState:
    def eventId = state.eventId
    def rawState = state

  private final case class InCommittedEventsSection(
    journalHeader: JournalHeader,
    var rawState: S,
    var eventId: EventId)
  extends HasState:
    def state = rawState.withEventId(eventId)

  private final case class InTransaction(journalHeader: JournalHeader, state: S)
  extends HasState:
    def eventId = state.eventId
    def rawState = state

    var stampedKeyedEvents = new mutable.ArrayBuffer[Stamped[KeyedEvent[Event]]]

    def add(stamped: Stamped[KeyedEvent[Event]]): Unit =
      stampedKeyedEvents = stampedKeyedEvents.nn :+ stamped


object FileSnapshotableStateRecoverer:
  private val logger = Logger[this.type]
