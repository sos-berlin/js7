package js7.journal.recover

import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.cluster.ClusterState
import js7.data.event.JournalSeparators.{Commit, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournalState, KeyedEvent, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.journal.recover.FileSnapshotableStateBuilder.*
import js7.journal.recover.JournalProgress.{AfterHeader, AfterSnapshotSection, InCommittedEventsSection, InSnapshotSection, InTransaction, Initial}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class FileSnapshotableStateBuilder[S <: SnapshotableState[S]](
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId],
  newBuilder: () => SnapshotableStateBuilder[S])
  (implicit S: SnapshotableState.Companion[S]):

  def this(journalFileForInfo: Path, expectedJournalId: Option[JournalId])
    (implicit S: SnapshotableState.Companion[S])
  = this(journalFileForInfo, expectedJournalId, S.newBuilder _)

  private val builder = newBuilder()
  private var _progress: JournalProgress = JournalProgress.Initial

  private object transaction:
    var buffer: ArrayBuffer[Stamped[KeyedEvent[Event]]] = null

    def begin(): Unit =
      require(!isInTransaction)
      buffer = new mutable.ArrayBuffer[Stamped[KeyedEvent[Event]]]

    def add(stamped: Stamped[KeyedEvent[Event]]): Unit =
      require(isInTransaction)
      buffer += stamped

    def clear(): Unit =
      buffer = null

    private def isInTransaction = buffer != null

  def startWithState(
    progress: JournalProgress,
    journalHeader: Option[JournalHeader],
    eventId: EventId,
    totalEventCount: Long,
    state: S)
  : Unit =
    this._progress = progress
    builder.initializeState(journalHeader, eventId, totalEventCount, state)

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
            builder.onAllSnapshotsAdded()
            _progress = AfterSnapshotSection
          case _ =>
            builder.addSnapshotObject(journalRecord)

      case AfterSnapshotSection =>
        if journalRecord != EventHeader then throw new IllegalArgumentException(
          "Missing EventHeader in journal file")
        _progress = InCommittedEventsSection

      case InCommittedEventsSection =>
        journalRecord match
          case Transaction =>
            transaction.begin()
            _progress = InTransaction
          case _ =>
            builder.addEvent(cast[Stamped[KeyedEvent[Event]]](journalRecord))

      case InTransaction =>
        journalRecord match
          case Commit =>
            _progress = InCommittedEventsSection
            for stamped <- transaction.buffer do
              builder.addEvent(stamped)
            transaction.clear()
          case _ =>
            transaction.add(cast[Stamped[KeyedEvent[Event]]](journalRecord))

      //case _ =>
      //  throw new IllegalArgumentException(
      //    s"Illegal JSON while journal file reader is in state '$journalProgress': ${journalRecord.toString.truncateWithEllipsis(100)}")

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
    builder.nextJournalHeader

  def eventId: EventId =
    builder.eventId

  def result(): S =
    builder.result()

  def journalState: JournalState =
    builder.journalState

  def clusterState: ClusterState =
    builder.clusterState

  def logStatistics(): Unit =
    builder.logStatistics(Try(Files.size(journalFileForInfo)).toOption)


object FileSnapshotableStateBuilder:
  private val logger = Logger[this.type]
