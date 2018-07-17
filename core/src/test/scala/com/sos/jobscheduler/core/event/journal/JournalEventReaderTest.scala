package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.HistoricJournalEventReaderTest.writeJournal
import com.sos.jobscheduler.core.event.journal.JournalEventReaderTest._
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, Stamped, TearableEventSeq}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class JournalEventReaderTest extends FreeSpec with BeforeAndAfterAll {

  private implicit lazy val timerService = TimerService(idleTimeout = Some(1.s))

  "eventReader.when, .onEventsAcceptedUntil" in {
    withJournalMeta { journalMeta ⇒
      writeJournal(journalMeta, EventId.BeforeFirst, TestEvents1)
      withJournal(journalMeta, TestEvents1.last.eventId) { (writer, eventReader) ⇒
        def when(after: EventId) = eventReader.when(EventRequest.singleClass[TestEvent](after = after, timeout = 30.seconds)).await(99.s).strict

        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(TestEvents1))

        writer.writeEvents(TestEvents2)
        writer.flush()
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(TestEvents1 ++ TestEvents2))
        assert(when(11) == EventSeq.NonEmpty(TestEvents1.tail ++ TestEvents2))
        assert(when(12) == EventSeq.NonEmpty(TestEvents2))
        assert(when(21) == EventSeq.NonEmpty(TestEvents2.tail))
        assert(eventReader.when(EventRequest.singleClass[TestEvent](after = 22, timeout = 10.millis)).await(99.s).strict == EventSeq.Empty(22))

        eventReader.onEventsAcceptedUntil(0)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(0), journalMeta.file(12)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(TestEvents1 ++ TestEvents2))

        eventReader.onEventsAcceptedUntil(11)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(0), journalMeta.file(12)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(TestEvents1 ++ TestEvents2))

        eventReader.onEventsAcceptedUntil(12)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(12)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(12))

        eventReader.onEventsAcceptedUntil(22)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(12)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(12))
      }
    }
  }

  "Read/write synchronization crash test" in {
    pending   // TODO Intensiv schreiben und lesen, um Synchronisation zu prüfen
    // Mit TakeSnapshot prüfen
  }

  private def withJournalMeta(body: JournalMeta[TestEvent] ⇒ Unit): Unit =
    withTemporaryDirectory("JournalEventReaderTest") { directory ⇒
      val journalMeta = JournalMeta(
        TypedJsonCodec(),  // No snapshots
        TestJsonCodecs.TestKeyedEventJsonCodec,
        directory / "test")
      body(journalMeta)
    }

  private def withJournal(journalMeta: JournalMeta[TestEvent], lastEventId: EventId)(body: (JournalWriter[TestEvent], JournalEventReader[TestEvent]) ⇒ Unit): Unit = {
    autoClosing(new JournalEventReader[TestEvent](journalMeta)) { eventReader ⇒
      autoClosing(new JournalWriter[TestEvent](journalMeta, after = lastEventId, Some(eventReader))) { writer ⇒
        writer.beginSnapshotSection()
        writer.beginEventSection()
        val reader = new JournalReader[TestEvent](journalMeta, journalMeta.file(after = lastEventId))
        assert(reader.recoverNext().isEmpty)
        body(writer, eventReader)
      }
    }
  }
}

private object JournalEventReaderTest
{
  private val TestEvents1 =
    Stamped(11, "A" <-: TestEvent.Appended('a')) ::
    Stamped(12, "B" <-: TestEvent.Removed) ::
    Nil

  private val TestEvents2 =
    Stamped(21, "C" <-: TestEvent.Appended('a')) ::
    Stamped(22, "D" <-: TestEvent.Removed) ::
    Nil
}
