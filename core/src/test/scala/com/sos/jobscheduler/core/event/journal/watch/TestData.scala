package com.sos.jobscheduler.core.event.journal.watch

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJsonObject
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.event.JournalEvent.SnapshotTaken
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, JournalEvent, JournalHeader, JournalId, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}
import java.nio.file.Path
import java.util.UUID

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

  val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[Event](
    KeyedSubtype[JournalEvent],
    KeyedSubtype[TestEvent])

  def writeJournalSnapshot[E <: Event](journalMeta: JournalMeta, after: EventId, snapshotObjects: Seq[Any]): Path =
    autoClosing(SnapshotJournalWriter.forTest(journalMeta, after = after)) { writer =>
      writer.writeHeader(JournalHeader.forTest(journalId, eventId = after))
      writer.beginSnapshotSection()
      for (o <- snapshotObjects) {
        writer.writeSnapshot(ByteString(journalMeta.snapshotJsonCodec.encodeObject(o).compactPrint))
      }
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(after + 1, NoKey <-: SnapshotTaken))
      writer.file
    }

  def writeJournal(journalMeta: JournalMeta, after: EventId, stampedEvents: Seq[Stamped[KeyedEvent[Event]]],
    journalId: JournalId = this.journalId): Path
  =
    autoClosing(EventJournalWriter.forTest(journalMeta, after = after, journalId)) { writer =>
      writer.writeHeader(JournalHeader.forTest(journalId, eventId = after))
      writer.beginEventSection(sync = false)
      writer.writeEvents(stampedEvents take 1)
      writer.writeEvents(stampedEvents drop 1 take 2, transaction = true)
      writer.writeEvents(stampedEvents drop 3)
      writer.endEventSection(sync = false)
      writer.file
    }
}
