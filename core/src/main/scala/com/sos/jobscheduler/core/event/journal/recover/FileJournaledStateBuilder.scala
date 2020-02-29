package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{Commit, EventFooter, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.recover.FileJournaledStateBuilder._
import com.sos.jobscheduler.core.event.journal.recover.JournalProgress.{AfterEventsSection, AfterHeader, AfterSnapshotSection, InEventsSection, InSnapshotSection, InTransaction, Initial}
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Joacim Zschimmer
  */
final class FileJournaledStateBuilder[S <: JournaledState[S, E], E <: Event](
  journalMeta: JournalMeta,
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId],
  newBuilder: () => JournaledStateBuilder[S, E])
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

    def add(positionAndStamped: Stamped[KeyedEvent[Event]]): Unit = {
      require(isInTransaction)
      buffer += positionAndStamped
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
        _progress = InEventsSection

      case InEventsSection =>
        json match {
          case EventFooter =>
            _progress = AfterEventsSection
          case Transaction =>
            transaction.begin()
            _progress = InTransaction
          case _ =>
            builder.addEvent(deserialize(json))
       }

      case InTransaction =>
        if (json == Commit) {
          _progress = InEventsSection
          for (stamped <- transaction.buffer) {
            builder.addEvent(stamped)
          }
          transaction.clear()
        } else {
          transaction.add(deserialize(json))
        }

      case _ =>
        throw new IllegalArgumentException(s"Illegal JSON while journal file reader is in state '$recovererState': ${json.compactPrint.truncateWithEllipsis(100)}")
    }

  def recovererState = _progress

  def fileJournalHeader = builder.fileJournalHeader

  def calculatedJournalHeader = builder.recoveredJournalHeader

  def eventId = builder.eventId

  def state = builder.state

  def clusterState = builder.clusterState

  def result: S =
    _progress match {
      case InEventsSection | AfterEventsSection =>
        builder.state
      case _ => throw new IllegalStateException(s"Journal file '$journalFileForInfo' is truncated in state '$recovererState'")
    }

  def isAcceptingEvents = _progress.isAcceptingEvents

  def logStatistics() = builder.logStatistics()

  private def deserialize(json: Json): Stamped[KeyedEvent[E]] = {
    import journalMeta.eventJsonCodec
    json.as[Stamped[KeyedEvent[Event]]]
      .orThrow { t =>
        val msg = s"Unexpected JSON: ${(t: io.circe.DecodingFailure).message}"
        logger.error(s"$msg: ${json.compactPrint}")
        new IllegalArgumentException(msg)
      }
      .asInstanceOf[Stamped[KeyedEvent[E]]]
  }
}

object FileJournaledStateBuilder
{
  private val logger = Logger(getClass)
}
