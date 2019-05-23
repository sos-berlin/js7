package com.sos.jobscheduler.core.event.journal.watch

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJsonObject
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}
import java.nio.file.Path
import java.util.UUID
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[watch] object TestData
{
  val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))

  sealed trait TestEvent extends Event {
    type Key = String
  }

  final case object AEvent extends TestEvent
  final case object BEvent extends TestEvent

  implicit val jsonFormat = TypedJsonCodec[TestEvent](
    Subtype(AEvent),
    Subtype(BEvent))

  val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[TestEvent](
    KeyedSubtype[TestEvent])

  def writeJournalSnapshot[E <: Event](journalMeta: JournalMeta[E], after: EventId, snapshotObjects: Seq[Any]): Unit =
    autoClosing(SnapshotJournalWriter.forTest[E](journalMeta, after = after)) { writer =>
      writer.writeHeader(JournalHeader(journalId, eventId = after, totalEventCount = 0))
      writer.beginSnapshotSection()
      for (o <- snapshotObjects) {
        writer.writeSnapshot(ByteString(journalMeta.snapshotJsonCodec.encodeObject(o).compactPrint))
      }
      writer.endSnapshotSection(sync = false)
    }

  def writeJournal[E <: Event](journalMeta: JournalMeta[E], after: EventId, stampedEvents: Seq[Stamped[KeyedEvent[E]]],
    journalId: JournalId = this.journalId): Path
  =
    autoClosing(EventJournalWriter.forTest[E](journalMeta, after = after)) { writer =>
      writer.writeHeader(JournalHeader(journalId, eventId = after, totalEventCount = 0))
      writer.beginEventSection()
      writer.writeEvents(stampedEvents take 1)
      writer.writeEvents(stampedEvents drop 1 take 2, transaction = true)
      writer.writeEvents(stampedEvents drop 3)
      writer.endEventSection(sync = false)
      writer.file
    }
}
