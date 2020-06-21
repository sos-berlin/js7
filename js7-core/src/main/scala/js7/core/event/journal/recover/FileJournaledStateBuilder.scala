package js7.core.event.journal.recover

import cats.syntax.show.toShow
import io.circe.Json
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.Logger
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.recover.FileJournaledStateBuilder._
import js7.core.event.journal.recover.JournalProgress.{AfterHeader, AfterSnapshotSection, InCommittedEventsSection, InSnapshotSection, InTransaction, Initial}
import js7.data.event.JournalSeparators.{Commit, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournaledState, JournaledStateBuilder, KeyedEvent, Stamped}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Joacim Zschimmer
  */
final class FileJournaledStateBuilder[S <: JournaledState[S]](
  journalMeta: JournalMeta,
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId],
  newBuilder: () => JournaledStateBuilder[S])
{
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

  def put(json: Json): Unit =
    _progress match {
      case Initial =>
        val header = JournalHeader.checkedHeader(json, journalFileForInfo, expectedJournalId).orThrow
        builder.addSnapshot(header)
        logger.debug(header.toString)
        _progress = AfterHeader

      case AfterHeader =>
        if (json != SnapshotHeader) throw new IllegalArgumentException("Missing SnapshotHeader in journal file")
        _progress = InSnapshotSection

      case InSnapshotSection =>
        if (json == SnapshotFooter) {
          builder.onAllSnapshotsAdded()
          _progress = AfterSnapshotSection
        } else {
          builder.addSnapshot(journalMeta.snapshotJsonCodec.decodeJson(json).orThrow)
        }

      case AfterSnapshotSection =>
        if (json != EventHeader) throw new IllegalArgumentException("Missing EventHeader in journal file")
        _progress = InCommittedEventsSection

      case InCommittedEventsSection =>
        json match {
          case Transaction =>
            transaction.begin()
            _progress = InTransaction
          case _ =>
            builder.addEvent(deserialize(json))
       }

      case InTransaction =>
        if (json == Commit) {
          _progress = InCommittedEventsSection
          for (stamped <- transaction.buffer) {
            builder.addEvent(stamped)
          }
          transaction.clear()
        } else {
          transaction.add(deserialize(json))
        }

      case _ =>
        throw new IllegalArgumentException(s"Illegal JSON while journal file reader is in state '$journalProgress': ${json.compactPrint.truncateWithEllipsis(100)}")
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

  def logStatistics() = builder.logStatistics()

  private def deserialize(json: Json): Stamped[KeyedEvent[Event]] = {
    import journalMeta.eventJsonCodec
    json.as[Stamped[KeyedEvent[Event]]]
      .orThrow { t =>
        val msg = s"Unexpected JSON: ${(t: io.circe.DecodingFailure).show}"
        logger.error(s"$msg: ${json.compactPrint}")
        new IllegalArgumentException(msg)
      }
      .asInstanceOf[Stamped[KeyedEvent[Event]]]
  }
}

object FileJournaledStateBuilder
{
  private val logger = Logger(getClass)
}
