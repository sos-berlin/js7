package js7.journal.watch

import cats.effect.unsafe.IORuntime
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
import js7.journal.data.JournalLocation
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}

/**
  * @author Joacim Zschimmer
  */
private[watch] object TestData:

  val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))

  sealed trait TestEvent extends Event.IsKeyBase[TestEvent]:
    val keyCompanion: TestEvent.type = TestEvent
  object TestEvent extends Event.CompanionForKey[String, TestEvent]:
    implicit val implicitSelf: TestEvent.type = this
  case object AEvent extends TestEvent
  case object BEvent extends TestEvent

  object TestState extends SnapshotableState.HasCodec:
    val name = "TestState"

    implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] = TypedJsonCodec(
      Subtype(AEvent),
      Subtype(BEvent))

    implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] = KeyedEventTypedJsonCodec(
      KeyedSubtype.singleton(using TestEvent)(AEvent),
      KeyedSubtype.singleton(using TestEvent)(BEvent))

  def writeJournalSnapshot[E <: Event](journalLocation: JournalLocation, after: EventId, snapshotObjects: Seq[Any])
    (using IORuntime): Path =
    autoClosing(SnapshotJournalWriter.forTest(journalLocation, after = after)) { writer =>
      writer.writeHeader(JournalHeaders.forTest(TestState.name, journalId, eventId = after))
      writer.beginSnapshotSection()
      for o <- snapshotObjects do
        writer.writeSnapshot(ByteArray(journalLocation.snapshotObjectJsonCodec.encodeObject(o).compactPrint))
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(after + 1, NoKey <-: SnapshotTaken))
      writer.file
    }

  def writeJournal(journalLocation: JournalLocation, after: EventId, stampedEvents: Seq[Stamped[KeyedEvent[Event]]],
    journalId: JournalId = this.journalId)
    (using IORuntime)
  : Path =
    autoClosing(EventJournalWriter.forTest(journalLocation, after = after, journalId)) { writer =>
      writer.writeHeader(JournalHeaders.forTest(TestState.name, journalId, eventId = after))
      writer.beginEventSection(sync = false)
      writer.writeEvents(stampedEvents take 1)
      writer.writeEvents(stampedEvents drop 1 take 2, transaction = true)
      writer.writeEvents(stampedEvents drop 3)
      writer.endEventSection(sync = false)
      writer.file
    }
