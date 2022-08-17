package js7.journal.watch

import java.nio.file.Path
import java.util.UUID
import js7.base.circeutils.CirceUtils.RichJsonObject
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.data.ByteArray
import js7.base.utils.AutoClosing.autoClosing
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalHeaders, JournalId, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotableState, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
private[watch] object TestData
{
  val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))

  sealed trait TestEvent extends Event {
    type Key = String
  }

  case object AEvent extends TestEvent
  case object BEvent extends TestEvent

  object TestState extends SnapshotableState.HasCodec {
    implicit val snapshotObjectJsonCodec = TypedJsonCodec(
      Subtype(AEvent),
      Subtype(BEvent))

    implicit val keyedEventJsonCodec = KeyedEventTypedJsonCodec(
      KeyedSubtype(AEvent),
      KeyedSubtype(BEvent))
  }

  def writeJournalSnapshot[E <: Event](journalMeta: JournalMeta, after: EventId, snapshotObjects: Seq[Any]): Path =
    autoClosing(SnapshotJournalWriter.forTest(journalMeta, after = after)) { writer =>
      writer.writeHeader(JournalHeaders.forTest(journalId, eventId = after))
      writer.beginSnapshotSection()
      for (o <- snapshotObjects) {
        writer.writeSnapshot(ByteArray(journalMeta.snapshotObjectJsonCodec.encodeObject(o).compactPrint))
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
      writer.writeHeader(JournalHeaders.forTest(journalId, eventId = after))
      writer.beginEventSection(sync = false)
      writer.writeEvents(stampedEvents take 1)
      writer.writeEvents(stampedEvents drop 1 take 2, transaction = true)
      writer.writeEvents(stampedEvents drop 3)
      writer.endEventSection(sync = false)
      writer.file
    }
}
