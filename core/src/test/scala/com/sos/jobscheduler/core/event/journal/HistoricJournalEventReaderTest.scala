package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.core.event.journal.HistoricJournalEventReaderTest._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class HistoricJournalEventReaderTest extends FreeSpec
{
  "eventsAfter" in {
    withTemporaryDirectory("HistoricJournalEventReaderTest-") { dir ⇒
      val journalMeta = TestMeta.testJournalMeta(dir resolve "test")

      autoClosing(new JournalWriter[TestEvent](journalMeta, after = After)) { writer ⇒
        writer.beginSnapshotSection()
        writer.beginEventSection()
        writer.writeEvents(TestEvents)
        writer.flush()
      }

      autoClosing(new HistoricJournalEventReader[TestEvent](journalMeta, tornEventId = After, journalMeta.file(After))) { reader ⇒
        locally {
          val closeableIterator = reader.eventsAfter(After)
          assert(closeableIterator.toList == TestEvents)
          closeableIterator.close()
        }
        locally {
          val closeableIterator = reader.eventsAfter(After + 1)
          assert(closeableIterator.toList == TestEvents.tail)
          closeableIterator.close()
        }
        locally {
          val closeableIterator = reader.eventsAfter(After + 2)
          assert(closeableIterator.toList == Nil)
          closeableIterator.close()
        }
      }
    }
  }
}

private[journal] object HistoricJournalEventReaderTest {
  private val After = 1000
  private val TestEvents =
    Stamped(After + 1, "A" <-: TestEvent.Appended('a')) ::
    Stamped(After + 2, "B" <-: TestEvent.Removed) ::
    Nil

  def writeJournal[E <: Event](journalMeta: JournalMeta[E], after: EventId, stampedEvents: Seq[Stamped[KeyedEvent[E]]]): Unit =
    autoClosing(new JournalWriter[E](journalMeta, after = after)) { writer ⇒
      writer.beginSnapshotSection()
      writer.beginEventSection()
      writer.writeEvents(stampedEvents)
      writer.flush()
      writer.close()
    }
}
