package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{Commit, EventFooter, EventHeader, SnapshotFooter, Transaction}
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.recover.JournalFileStateBuilder._
import com.sos.jobscheduler.core.event.journal.recover.JournalRecovererState.{AfterEventsSection, AfterHeader, AfterSnapshotSection, InEventsSection, InSnapshotSection, InTransaction, Initial}
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Joacim Zschimmer
  */
final class JournalFileStateBuilder[S <: JournaledState[S, E], E <: Event](
  journalMeta: JournalMeta,
  journalFileForInfo: Path,
  expectedJournalId: Option[JournalId],
  newBuilder: () => JournalStateBuilder[S, E])
{
  private val builder = newBuilder()
  private var _recovererState: JournalRecovererState = JournalRecovererState.Initial

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

  def startWithState(recovererState: JournalRecovererState, journalHeader: Option[JournalHeader], eventId: EventId, totalEventCount: Long, state: S): Unit = {
    this._recovererState = recovererState
    builder.initializeState(journalHeader, eventId, totalEventCount, state)
  }

  def put(json: Json): Unit =
    _recovererState match {
      case Initial =>
        val header = JournalHeader.checkedHeader(json, journalFileForInfo, expectedJournalId).orThrow
        builder.addSnapshot(header)
        logger.debug(header.toString)
        _recovererState = AfterHeader

      case AfterHeader =>
        if (json != JournalSeparators.SnapshotHeader) throw new IllegalArgumentException("Missing SnapshotHeader in journal file")
        _recovererState = InSnapshotSection

      case InSnapshotSection =>
        if (json == SnapshotFooter) {
          builder.onAllSnapshotsAdded()
          _recovererState = AfterSnapshotSection
        } else {
          builder.addSnapshot(journalMeta.snapshotJsonCodec.decodeJson(json).orThrow)
        }

      case AfterSnapshotSection =>
        if (json != EventHeader) throw new IllegalArgumentException("Missing EventHeader in journal file")
        _recovererState = InEventsSection

      case InEventsSection =>
        json match {
          case EventFooter =>
            _recovererState = AfterEventsSection
          case Transaction =>
            transaction.begin()
            _recovererState = InTransaction
          case _ =>
            builder.addEvent(deserialize(json))
       }

      case InTransaction =>
        if (json == Commit) {
          _recovererState = InEventsSection
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

  def recovererState = _recovererState

  def fileJournalHeader = builder.fileJournalHeader

  def calculatedJournalHeader = builder.recoveredJournalHeader

  def eventId = builder.eventId

  def state = builder.state

  def clusterState = builder.clusterState

  def result: S =
    _recovererState match {
      case InEventsSection | AfterEventsSection =>
        builder.state
      case _ => throw new IllegalStateException(s"Journal file '$journalFileForInfo' is truncated in state '$recovererState'")
    }

  def isAcceptingEvents = _recovererState.isAcceptingEvents

  def logStatistics() = builder.logStatistics()

  private def deserialize(json: Json): Stamped[KeyedEvent[E]] = {
    import journalMeta.eventJsonCodec
    json.as[Stamped[KeyedEvent[Event]]]
      .orThrow
      .asInstanceOf[Stamped[KeyedEvent[E]]]
  }
}

object JournalFileStateBuilder
{
  private val logger = Logger(getClass)
}
