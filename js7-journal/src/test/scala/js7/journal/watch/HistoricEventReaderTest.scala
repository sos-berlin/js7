package js7.journal.watch

import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.io.file.FileUtils.*
import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.autoClosing
import js7.data.event.{JournalHeaders, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.watch.HistoricEventReaderTest.*
import js7.journal.watch.TestData.{AEvent, BEvent, TestKeyedEventJsonCodec, journalId}
import js7.journal.write.EventJournalWriter
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class HistoricEventReaderTest extends OurTestSuite
{
  "eventsAfter" in {
    withTemporaryDirectory("HistoricEventReaderTest-") { dir =>
      val journalMeta = JournalMeta(TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")

      autoClosing(EventJournalWriter.forTest(journalMeta, after = After, journalId)) { writer =>
        writer.writeHeader(JournalHeaders.forTest(journalId, eventId = After))
        writer.beginEventSection(sync = false)
        writer.writeEvents(TestEvents)
        writer.endEventSection(sync = false)
      }

      autoClosing(new HistoricEventReader(journalMeta, journalId, fileEventId = After, journalMeta.file(After), JournalEventWatch.TestConfig)) { reader =>
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
