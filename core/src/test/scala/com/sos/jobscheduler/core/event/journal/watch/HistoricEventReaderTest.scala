package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.HistoricEventReaderTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.{AEvent, BEvent, TestEvent, TestKeyedEventJsonCodec}
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter
import com.sos.jobscheduler.data.event.Stamped
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class HistoricEventReaderTest extends FreeSpec
{
  "eventsAfter" in {
    withTemporaryDirectory("HistoricEventReaderTest-") { dir ⇒
      val journalMeta = new JournalMeta[TestEvent](TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")

      autoClosing(EventJournalWriter.forTest[TestEvent](journalMeta, after = After)) { writer ⇒
        writer.startJournaling()
        writer.writeEvents(TestEvents)
        writer.flush(sync = false)
      }

      autoClosing(new HistoricEventReader[TestEvent](journalMeta, tornEventId = After, journalMeta.file(After), JournalEventWatch.TestConfig)) { reader ⇒
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

object HistoricEventReaderTest {
  private val After = 1000
  private val TestEvents =
    Stamped(After + 1, "A" <-: AEvent) ::
    Stamped(After + 2, "B" <-: BEvent) ::
    Nil

}
