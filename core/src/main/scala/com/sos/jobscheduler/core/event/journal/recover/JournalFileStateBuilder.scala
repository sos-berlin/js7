package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{Commit, EventFooter, EventHeader, SnapshotFooter, Transaction}
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.recover.JournalRecovererState.{AfterEventsSection, AfterHeader, AfterSnapshotSection, InEventsSection, InSnapshotSection, InTransaction, Start}
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, JournalId, JournaledState, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Joacim Zschimmer
  */
final class JournalFileStateBuilder[S <: JournaledState[S, E], E <: Event](
  journalMeta: JournalMeta,
  journalFile: Path,
  expectedJournalId: Option[JournalId],
  builder: JournalStateBuilder[S, E])
{
  private var recovererState = JournalRecovererState()

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

  def put(json: Json): Unit =
    recovererState match {
      case Start =>
        builder.addSnapshot(JournalHeader.checkedHeader(json, journalFile, expectedJournalId).orThrow)
        recovererState = AfterHeader

      case AfterHeader =>
        if (json != JournalSeparators.SnapshotHeader) throw new IllegalArgumentException("Missing SnapshotHeader in journal file")
        recovererState = InSnapshotSection

      case InSnapshotSection =>
        if (json == SnapshotFooter) {
          builder.onAllSnapshotsAdded()
          recovererState = AfterSnapshotSection
        } else {
          builder.addSnapshot(journalMeta.snapshotJsonCodec.decodeJson(json).orThrow)
        }

      case AfterSnapshotSection =>
        if (json != EventHeader) throw new IllegalArgumentException("Missing EventHeader in journal file")
          recovererState = InEventsSection

      case InEventsSection =>
        json match {
          case EventFooter =>
            recovererState = AfterEventsSection
          case Transaction =>
            transaction.begin()
            recovererState = InTransaction
          case _ =>
            addEvent(deserialize(json))
       }

      case InTransaction =>
        if (json == Commit) {
          recovererState = InEventsSection
          for (stamped <- transaction.buffer) {
            addEvent(stamped)
          }
          transaction.clear()
        } else {
          transaction.add(deserialize(json))
        }

      case _ =>
        throw new IllegalArgumentException(s"Illegal JSON while journal file reader is in state '$recovererState': ${json.compactPrint.truncateWithEllipsis(100)}")
    }

  private def addEvent(stamped: Stamped[KeyedEvent[Event]]): Unit =
    builder.addEvent(stamped)

  def result: S =
    recovererState match {
      case InEventsSection | AfterEventsSection =>
        builder.state
      case _ => throw new IllegalStateException(s"Journal file '$journalFile' is truncated in state '$recovererState'")
    }


  private def deserialize(json: Json): Stamped[KeyedEvent[E]] = {
    import journalMeta.eventJsonCodec
    json.as[Stamped[KeyedEvent[Event]]]
      .orThrow
      .asInstanceOf[Stamped[KeyedEvent[E]]]
  }
}
