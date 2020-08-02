package js7.core.event.journal.watch

import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.utils.AutoClosing.autoClosing
import js7.common.scalautil.FileUtils._
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.files.JournalFiles.JournalMetaOps
import js7.core.event.journal.watch.HistoricEventReaderTest._
import js7.core.event.journal.watch.TestData.{AEvent, BEvent, TestKeyedEventJsonCodec, journalId}
import js7.core.event.journal.write.EventJournalWriter
import js7.data.event.{JournalHeader, Stamped}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class HistoricEventReaderTest extends AnyFreeSpec
{
  "eventsAfter" in {
    withTemporaryDirectory("HistoricEventReaderTest-") { dir =>
      val journalMeta = JournalMeta(TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")

      autoClosing(EventJournalWriter.forTest(journalMeta, after = After, journalId)) { writer =>
        writer.writeHeader(JournalHeader.forTest(journalId, eventId = After))
        writer.beginEventSection(sync = false)
        writer.writeEvents(TestEvents)
        writer.endEventSection(sync = false)
      }

      autoClosing(new HistoricEventReader(journalMeta, Some(journalId), tornEventId = After, journalMeta.file(After), JournalEventWatch.TestConfig)) { reader =>
        assert(reader.eventsAfter(After + 5) == None)
        assert(reader.eventsAfter(After + 15) == None)
        assert(reader.eventsAfter(After + 25) == None)
        locally {
          val Some(closeableIterator) = reader.eventsAfter(After)
          assert(closeableIterator.toList == TestEvents)
          closeableIterator.close()
        }
        locally {
          val Some(closeableIterator) = reader.eventsAfter(After + 10)
          assert(closeableIterator.toList == TestEvents.tail)
          closeableIterator.close()
        }
        locally {
          val Some(closeableIterator) = reader.eventsAfter(After + 20)
          assert(closeableIterator.toList == Nil)
          closeableIterator.close()
        }
      }
    }
  }
}

object HistoricEventReaderTest {
  private val After = 1000
  private val TestEvents =
    Stamped(After + 10, "A" <-: AEvent) ::
    Stamped(After + 20, "B" <-: BEvent) ::
    Nil
}
