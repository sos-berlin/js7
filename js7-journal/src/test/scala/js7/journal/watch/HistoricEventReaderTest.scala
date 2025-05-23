package js7.journal.watch

import cats.effect.unsafe.IORuntime
import js7.base.io.file.FileUtils.*
import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.autoClosing
import js7.data.event.{JournalHeader, Stamped}
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.watch.HistoricEventReaderTest.*
import js7.journal.watch.TestData.{AEvent, BEvent, TestState, journalId}
import js7.journal.write.EventJournalWriter

/**
  * @author Joacim Zschimmer
  */
final class HistoricEventReaderTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "eventsAfter" in:
    withTemporaryDirectory("HistoricEventReaderTest-") { dir =>
      val journalLocation = JournalLocation(TestState, dir.resolve("test"))

      autoClosing(EventJournalWriter.forTest(journalLocation, after = After, journalId)) { writer =>
        writer.writeHeader(JournalHeader.forTest(TestState.name, journalId, eventId = After))
        writer.beginEventSection(sync = false)
        writer.writeEvents(TestEvents)
        writer.endEventSection(sync = false)
      }

      val historicEventReader = new HistoricEventReader(journalLocation, journalId,
        fileEventId = After, journalLocation.file(After), JournalEventWatch.TestConfig, ioRuntime)

      autoClosing(historicEventReader) { reader =>
        assert(reader.eventsAfter(After + 5) == None)
        assert(reader.eventsAfter(After + 15) == None)
        assert(reader.eventsAfter(After + 25) == None)
        locally:
          val Some(closeableIterator) = reader.eventsAfter(After): @unchecked
          assert(closeableIterator.toList == TestEvents)
          closeableIterator.close()
        locally:
          val Some(closeableIterator) = reader.eventsAfter(After + 10): @unchecked
          assert(closeableIterator.toList == TestEvents.tail)
          closeableIterator.close()
        locally:
          val Some(closeableIterator) = reader.eventsAfter(After + 20): @unchecked
          assert(closeableIterator.toList == Nil)
          closeableIterator.close()
      }
    }


object HistoricEventReaderTest:
  private val After = 1000
  private val TestEvents =
    Stamped(After + 10, "A" <-: AEvent) ::
    Stamped(After + 20, "B" <-: BEvent) ::
    Nil
