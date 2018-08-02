package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.HistoricJournalEventReaderTest._
import com.sos.jobscheduler.core.event.journal.write.JournalWriter
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class HistoricJournalEventReaderTest extends FreeSpec
{
  "eventsAfter" in {
    withTemporaryDirectory("HistoricJournalEventReaderTest-") { dir ⇒
      val journalMeta = new JournalMeta[TestEvent](TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")

      autoClosing(new JournalWriter[TestEvent](journalMeta, after = After)) { writer ⇒
        writer.beginSnapshotSection()
        writer.startJournaling()
        writer.writeEvents(TestEvents)
        writer.flush(sync = false)
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

object HistoricJournalEventReaderTest {
  private[journal] sealed trait TestEvent extends Event {
    type Key = String
  }

  private final case object AEvent extends TestEvent
  private final case object BEvent extends TestEvent

  private implicit val jsonFormat = TypedJsonCodec[TestEvent](
    Subtype(AEvent),
    Subtype(BEvent))

  private val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[TestEvent](
    KeyedSubtype[TestEvent])

  private val After = 1000
  private val TestEvents =
    Stamped(After + 1, "A" <-: AEvent) ::
    Stamped(After + 2, "B" <-: BEvent) ::
    Nil

  def writeJournal[E <: Event](journalMeta: JournalMeta[E], after: EventId, stampedEvents: Seq[Stamped[KeyedEvent[E]]]): Unit =
    autoClosing(new JournalWriter[E](journalMeta, after = after)) { writer ⇒
      writer.beginSnapshotSection()
      writer.startJournaling()
      writer.writeEvents(stampedEvents)
      writer.flush(sync = false)
      writer.close()
    }
}
