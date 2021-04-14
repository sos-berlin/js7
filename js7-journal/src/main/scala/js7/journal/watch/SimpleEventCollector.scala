package js7.journal.watch

import com.typesafe.config.{Config, ConfigFactory}
import io.circe.{Decoder, Encoder}
import java.nio.file.Files.createTempDirectory
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{AnyKeyedEvent, Event, EventId, JournalHeaders, JournalId, KeyedEventTypedJsonCodec, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.write.EventJournalWriter
import monix.execution.Scheduler
import scala.reflect.ClassTag

final class SimpleEventCollector(
  eventTypedJsonCodec: KeyedEventTypedJsonCodec[Event],
  config: Config = ConfigFactory.empty)
(implicit protected val scheduler: Scheduler)
extends AutoCloseable
{
  private val directory = createTempDirectory("SimpleEventCollector-")
  private val journalMeta = JournalMeta(TypedJsonCodec[Any](), eventTypedJsonCodec, directory / "TEST")
  private val journalId = JournalId.random()
  private val tornEventId = EventId.BeforeFirst

  lazy val (eventWriter, eventWatch) = {
    // Start the other when accessing one.
    val eventWatch = new JournalEventWatch(journalMeta, config withFallback JournalEventWatch.TestConfig)
    val eventWriter = {
      val w = EventJournalWriter.forTest(journalMeta, tornEventId, journalId, Some(eventWatch))
      w.writeHeader(JournalHeaders.forTest(journalId, tornEventId))
      w.beginEventSection(sync = false)
      w.onJournalingStarted()
      w
    }
    (eventWriter, eventWatch)
  }

  def close(): Unit = {
    eventWriter.close()
    deleteDirectoryRecursively(directory)
  }

  def addStamped(stamped: Stamped[AnyKeyedEvent]): Unit = {
    eventWriter.writeEvent(stamped)
    eventWriter.flush(sync = false)
    eventWriter.onCommitted(eventWriter.fileLengthAndEventId, n = 1)
  }
}

object SimpleEventCollector
{
  def apply[E <: Event: ClassTag](config: Config = ConfigFactory.empty)
  (implicit ke: Encoder[E#Key], kd: Decoder[E#Key], codec: TypedJsonCodec[E], scheduler: Scheduler)
  : SimpleEventCollector = {
    val keyedEventTypedJsonCodec = KeyedEventTypedJsonCodec[Event](KeyedSubtype[E])
    new SimpleEventCollector(keyedEventTypedJsonCodec, config)
  }
}
