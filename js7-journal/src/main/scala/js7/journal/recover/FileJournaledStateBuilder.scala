package js7.journal.recover

import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.event.JournalSeparators.{Commit, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournaledState, JournaledStateBuilder, KeyedEvent, Stamped}
import js7.journal.recover.FileJournaledStateBuilder._
import js7.journal.recover.JournalProgress.{AfterHeader, AfterSnapshotSection, InCommittedEventsSection, InSnapshotSection, InTransaction, Initial}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class FileJournaledStateBuilder[S <: JournaledState[S]](
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId],
  newBuilder: () => JournaledStateBuilder[S])
{
  def this(journalFileForInfo: Path, expectedJournalId: Option[JournalId])(implicit S: JournaledState.Companion[S]) =
    this(journalFileForInfo, expectedJournalId, S.newBuilder _)

  private val builder = newBuilder()
  private var _progress: JournalProgress = JournalProgress.Initial

  private object transaction
  {
    var buffer: ArrayBuffer[Stamped[KeyedEvent[Event]]] = null

    def begin(): Unit = {
      require(!isInTransaction)
      buffer = new mutable.ArrayBuffer[Stamped[KeyedEvent[Event]]]
    }

    def add(stamped: Stamped[KeyedEvent[Event]]): Unit = {
      require(isInTransaction)
      buffer += stamped
    }

    def clear(): Unit =
      buffer = null

    private def isInTransaction = buffer != null
  }

  def startWithState(progress: JournalProgress, journalHeader: Option[JournalHeader], eventId: EventId, totalEventCount: Long, state: S): Unit = {
    this._progress = progress
    builder.initializeState(journalHeader, eventId, totalEventCount, state)
  }

  def put(journalRecord: Any): Unit =
    _progress match {
      case Initial =>
        journalRecord match {
          case journalHeader: JournalHeader =>
            logger.debug(journalHeader.toString)
            JournalHeader.checkedHeader(journalHeader, journalFileForInfo, expectedJournalId)
              .orThrow
            builder.addSnapshotObject(journalHeader)
            _progress = AfterHeader

          case _ => throw new IllegalArgumentException(
            s"Not a valid JS7 journal file: $journalFileForInfo. Expected a JournalHeader instead of " +
              s"${journalRecord.toString.truncateWithEllipsis(100)}:")
        }

      case AfterHeader =>
        if (journalRecord != SnapshotHeader) throw new IllegalArgumentException("Missing SnapshotHeader in journal file")
        _progress = InSnapshotSection

      case InSnapshotSection =>
        journalRecord match {
          case SnapshotFooter =>
            builder.onAllSnapshotsAdded()
            _progress = AfterSnapshotSection
          case _ =>
            builder.addSnapshotObject(journalRecord)
        }

      case AfterSnapshotSection =>
        if (journalRecord != EventHeader) throw new IllegalArgumentException("Missing EventHeader in journal file")
        _progress = InCommittedEventsSection

      case InCommittedEventsSection =>
        journalRecord match {
          case Transaction =>
            transaction.begin()
            _progress = InTransaction
          case _ =>
            builder.addEvent(cast[Stamped[KeyedEvent[Event]]](journalRecord))
       }

      case InTransaction =>
        journalRecord match {
          case Commit =>
            _progress = InCommittedEventsSection
            for (stamped <- transaction.buffer) {
              builder.addEvent(stamped)
            }
            transaction.clear()
          case _ =>
            transaction.add(cast[Stamped[KeyedEvent[Event]]](journalRecord))
        }

      case _ =>
        throw new IllegalArgumentException(s"Illegal JSON while journal file reader is in state '$journalProgress': ${journalRecord.toString.truncateWithEllipsis(100)}")
    }

  def rollbackToEventSection(): Unit =
    _progress match {
      case InCommittedEventsSection =>
      case InTransaction =>
        rollback()
      case _ =>
        throw new IllegalStateException(s"rollbackToEventSection() but progress=${_progress}")
    }

  private def rollback(): Unit = {
    transaction.clear()
    _progress = InCommittedEventsSection
  }

  def journalProgress = _progress

  def fileJournalHeader = builder.fileJournalHeader

  def calculatedJournalHeader = builder.recoveredJournalHeader

  def eventId = builder.eventId

  def state = builder.state

  def journalState = builder.journalState

  def clusterState = builder.clusterState

  def result: S =
    _progress match {
      case InCommittedEventsSection => builder.state
      case _ => throw new IllegalStateException(s"Journal file '$journalFileForInfo' is truncated in state '$journalProgress'")
    }

  def isAcceptingEvents = _progress.isAcceptingEvents

  def logStatistics(): Unit =
    builder.logStatistics(Try(Files.size(journalFileForInfo)).toOption)
}

object FileJournaledStateBuilder
{
  private val logger = Logger(getClass)
}
