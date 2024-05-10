package js7.journal.watch

import cats.effect.unsafe.IORuntime
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.{Decoder, Encoder}
import java.nio.file.Files.createTempDirectory
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{AnyKeyedEvent, Event, EventId, JournalHeaders, JournalId, KeyedEventTypedJsonCodec, SnapshotableState, Stamped}
import js7.journal.data.JournalLocation
import js7.journal.write.EventJournalWriter
import org.jetbrains.annotations.TestOnly
import scala.reflect.ClassTag

final class SimpleEventCollector(
  eventTypedJsonCodec: KeyedEventTypedJsonCodec[Event],
  config: Config = ConfigFactory.empty)
(implicit protected val ioRuntime: IORuntime)
extends AutoCloseable:

  private val directory = createTempDirectory("SimpleEventCollector-")
  private val journalLocation = JournalLocation(
    new SnapshotableState.HasCodec {
      implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] = eventTypedJsonCodec
      val name: String = "TestState"
      val snapshotObjectJsonCodec = TypedJsonCodec()
    },
    directory / "TEST")
  private val journalId = JournalId.random()
  private val tornEventId = EventId.BeforeFirst

  lazy val (eventWriter, eventWatch) =
    // Start the other when accessing one.
    val eventWatch = new JournalEventWatch(journalLocation, config withFallback JournalEventWatch.TestConfig)
    val eventWriter =
      val w = EventJournalWriter.forTest(journalLocation, tornEventId, journalId, Some(eventWatch))
      w.writeHeader(JournalHeaders.forTest("TestState", journalId, tornEventId))
      w.beginEventSection(sync = false)
      w.onJournalingStarted()
      w
    (eventWriter, eventWatch)

  def close(): Unit =
    eventWriter.close()
    deleteDirectoryRecursively(directory)

  @TestOnly
  def addStamped(stamped: Stamped[AnyKeyedEvent]): Unit =
    eventWriter.writeEvent(stamped)
    eventWriter.flush(sync = false)
    eventWriter.onCommitted(eventWriter.fileLengthAndEventId, n = 1)


object SimpleEventCollector:
  @TestOnly
  def apply[E <: Event: ClassTag](using E: Event.KeyCompanion[? >: E])(config: Config = ConfigFactory.empty)
  (implicit ke: Encoder[E.Key], kd: Decoder[E.Key], codec: TypedJsonCodec[E], ioRuntime: IORuntime)
  : SimpleEventCollector =
    val keyedEventTypedJsonCodec = KeyedEventTypedJsonCodec[Event](KeyedSubtype[E])
    new SimpleEventCollector(keyedEventTypedJsonCodec, config)
