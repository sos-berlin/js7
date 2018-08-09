package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.CirceUtils
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.event.RealEventWatch
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatchTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.writeJournal
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class JournalEventWatchTest extends FreeSpec with BeforeAndAfterAll {

  private implicit lazy val timerService = TimerService(idleTimeout = Some(1.s))

  "eventWatch.when, .onEventsAcceptedUntil" in {
    withJournalMeta { journalMeta ⇒
      writeJournal(journalMeta, EventId.BeforeFirst, MyEvents1)
      withJournal(journalMeta, MyEvents1.last.eventId) { (writer, eventWatch) ⇒
        def when(after: EventId) = eventWatch.when(EventRequest.singleClass[MyEvent](after = after, timeout = 30.seconds)).await(99.s).strict

        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))

        writer.writeEvents(MyEvents2)
        writer.flush(sync = false)
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))
        assert(when(110) == EventSeq.NonEmpty(MyEvents1.tail ++ MyEvents2))
        assert(when(120) == EventSeq.NonEmpty(MyEvents2))
        assert(when(210) == EventSeq.NonEmpty(MyEvents2.tail))
        assert(eventWatch.when(EventRequest.singleClass[MyEvent](after = 220, timeout = 10.millis)).await(99.s).strict == EventSeq.Empty(220))

        eventWatch.onEventsAcceptedUntil(0)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(0), journalMeta.file(120)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))

        eventWatch.onEventsAcceptedUntil(110)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(0), journalMeta.file(120)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))

        eventWatch.onEventsAcceptedUntil(120)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(120)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(120))

        eventWatch.onEventsAcceptedUntil(220)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(120)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(120))
      }
    }
  }

  "CurrentEventReader only" - {
    "eventWatch.when" in {
      withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) ⇒
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst).get.strict.isEmpty)

        val events = Stamped(1, "1" <-: A1) :: Stamped(2, "2" <-: A1) :: Nil
        writer.writeEvents(events)
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst).get.strict.isEmpty)  // Not flushed, so nothing has been read

        writer.flush(sync = false)
        val stampedEventSeq = eventWatch.eventsAfter(after = EventId.BeforeFirst).get.strict
        assert(stampedEventSeq == events)
        assert(eventWatch.eventsAfter(after = stampedEventSeq(0).eventId).get.strict == events.tail)
        assert(eventWatch.eventsAfter(after = stampedEventSeq(1).eventId).get.strict.isEmpty)

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](timeout = 30.seconds)).await(99.s).strict ==
          EventSeq.NonEmpty(events))
      }
    }

    "eventWatch.when with torn event stream" in {
      withJournalEventWatch(lastEventId = EventId(1000)) { (writer, eventWatch) ⇒
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst) == None)
        assert(eventWatch.eventsAfter(after = 999) == None)
        assert(eventWatch.eventsAfter(after = 1000).map(_.toVector) == Some(Nil))

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](timeout = 30.seconds)).await(99.s).strict ==
          TearableEventSeq.Torn(after = 1000))

        val anyFuture = eventWatch.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.seconds)).runAsync
        writer.writeEvents(Stamped(1001, "1" <-: A1) :: Nil)
        writer.flush(sync = false)
        val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
        assert(anyEvents == Stamped(1001, "1" <-: A1) :: Nil)
      }
    }

    "eventWatch.when for selected event subclasses" in {
      withJournalEventWatch(lastEventId = EventId(1000)) { (writer, eventWatch) ⇒
        val anyFuture = eventWatch.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.seconds)).runAsync
        val bFuture = eventWatch.when(EventRequest.singleClass[BEvent](after = 1000, timeout = 30.seconds)).runAsync

        writer.writeEvents(Stamped(1001, "1" <-: A1) :: Nil)
        writer.flush(sync = false)
        val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
        assert(anyEvents == Stamped(1001, "1" <-: A1) :: Nil)

        writer.writeEvents(Stamped(1002, "2" <-: B1) :: Nil)
        writer.flush(sync = false)
        val EventSeq.NonEmpty(bEventsIterator) = bFuture.await(99.s).strict
        assert(bEventsIterator == Stamped(1002, "2" <-: B1) :: Nil)

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.seconds)).await(99.s).strict ==
          EventSeq.NonEmpty(Stamped(1001, "1" <-: A1) :: Stamped(1002, "2" <-: B1) :: Nil))
      }
    }

    "eventWatch.whenKey, whenKeyedEvent" in {
      withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) ⇒
        writer.writeEvents(
          Stamped(1, "1" <-: A1) ::
          Stamped(2, "1" <-: B1) ::
          Stamped(3, "1" <-: A2) ::
          Stamped(4, "2" <-: A2) ::
          Stamped(5, "1" <-: B2) :: Nil)
        writer.flush(sync = false)

        def eventsForKey[E <: MyEvent: ClassTag](key: E#Key) = {
          val EventSeq.NonEmpty(eventIterator) = eventWatch.whenKey[E](EventRequest.singleClass(timeout = 99.seconds), key).await(10.s).strict
          eventIterator.toVector map { _.value }
        }
        assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
        assert(eventsForKey[AEvent]("2") == Vector(A2))
        assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

        def keyedEvent[E <: MyEvent: ClassTag](key: E#Key) =
          eventWatch.whenKeyedEvent[E](EventRequest.singleClass(timeout = 99.seconds), key) await 10.s
        assert(keyedEvent[AEvent]("1") == A1)
        assert(keyedEvent[AEvent]("2") == A2)
        assert(keyedEvent[BEvent]("1") == B1)
      }
    }

    "Second onJournalingStarted (snapshot)" in {
      withJournalMeta { journalMeta ⇒
        autoClosing(new JournalEventWatch[MyEvent](journalMeta, JournalEventWatch.TestConfig)) { eventWatch ⇒
          autoClosing(EventJournalWriter.forTest[MyEvent](journalMeta, after = EventId.BeforeFirst, Some(eventWatch))) { writer ⇒
            writer.startJournaling()
            writer.writeEvents(
              Stamped(1, "1" <-: A1) ::
              Stamped(2, "1" <-: B1) ::
              Stamped(3, "1" <-: A2) :: Nil)
            writer.flush(sync = false)
          }
          assert(eventWatch.historicFileEventIds == Set[EventId]())

          autoClosing(EventJournalWriter.forTest[MyEvent](journalMeta, after = 3, Some(eventWatch))) { writer ⇒
            writer.startJournaling()
            writer.writeEvents(
              Stamped(4, "2" <-: A2) ::
              Stamped(5, "1" <-: B2) :: Nil)
          }
          assert(eventWatch.historicFileEventIds == Set[EventId](0))
        }
      }
    }

    "observe" in {
      withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) ⇒
        val stampeds = mutable.Buffer[Stamped[KeyedEvent[AEvent]]]()
        val observed = eventWatch.observe(EventRequest.singleClass[AEvent](limit = 3, timeout = 99.seconds))
          .foreach(stampeds.+=)
        assert(stampeds.isEmpty)

        writer.writeEvents(Stamped(1, "1" <-: A1) :: Stamped(2, "2" <-: B1) :: Stamped(3, "3" <-: A1) :: Nil)
        writer.flush(sync = false)
        waitForCondition(99.s, 10.ms) { stampeds.size == 2 }
        assert(stampeds == Stamped(1, "1" <-: A1) :: Stamped(3, "3" <-: A1) :: Nil)

        writer.writeEvents(Stamped(4, "4" <-: A1) :: Nil)
        writer.flush(sync = false)
        waitForCondition(99.s, 10.ms) { stampeds.size == 3 }
        assert(stampeds == Stamped(1, "1" <-: A1) :: Stamped(3, "3" <-: A1) :: Stamped(4, "4" <-: A1) :: Nil)

        // limit=3 reached
        observed await 99.s
      }
    }

    "observe Torn" in {
      // Wie geben wir am besten 'Torn' zurück? Als Ende des Streams, als Exception oder als eigenes Objekt?
      withJournalEventWatch(lastEventId = EventId(1000)) { (_, eventWatch) ⇒
        val e = intercept[RealEventWatch.TornException] {
          eventWatch.observe(EventRequest.singleClass[AEvent](after = 10, timeout = 99.seconds)).countL await 9.s
        }
        assert(e.after == 10 && e.tornEventId == 1000)
      }
    }

    "observe after=(unknown EventId)" in {
      withJournalMeta { journalMeta ⇒
        withJournal(journalMeta, lastEventId = EventId(100)) { (writer, eventWatch) ⇒
          writer.writeEvents(MyEvents1)
          writer.flush(sync = false)

          val e = intercept[RealEventWatch.TornException] {
            eventWatch.observe(EventRequest.singleClass[AEvent](after = 115, timeout = 99.seconds)).countL await 9.s
          }
          assert(e.after == 115 && e.tornEventId == 100)

          val stampeds = mutable.Buffer[Stamped[KeyedEvent[AEvent]]]()
          eventWatch.observe(EventRequest.singleClass[AEvent](after = 110, timeout = 99.seconds)).foreach(stampeds.+=)
          assert(stampeds == MyEvents1(1) :: Nil)
        }
      }
    }
  }

  "Read/write synchronization crash test" in {
    pending   // TODO Intensiv schreiben und lesen, um Synchronisation zu prüfen
    // Mit TakeSnapshot prüfen
  }

  private def withJournalEventWatch(lastEventId: EventId)(body: (EventJournalWriter[MyEvent], JournalEventWatch[MyEvent]) ⇒ Unit): Unit = {
    withJournalMeta { journalMeta ⇒
      withJournal(journalMeta, lastEventId)(body)
    }
  }

  private def withJournalMeta(body: JournalMeta[MyEvent] ⇒ Unit): Unit =
    withTemporaryDirectory("JournalEventWatchTest") { directory ⇒
      val journalMeta = JournalMeta(
        TypedJsonCodec(),  // No snapshots
        keyedEventTypedJsonCodec,
        directory / "test")
      body(journalMeta)
    }

  private def withJournal(journalMeta: JournalMeta[MyEvent], lastEventId: EventId)(body: (EventJournalWriter[MyEvent], JournalEventWatch[MyEvent]) ⇒ Unit): Unit = {
    autoClosing(new JournalEventWatch[MyEvent](journalMeta, JournalEventWatch.TestConfig)) { eventWatch ⇒
      autoClosing(EventJournalWriter.forTest[MyEvent](journalMeta, after = lastEventId, Some(eventWatch))) { writer ⇒
        writer.startJournaling()
        body(writer, eventWatch)
      }
    }
  }
}

private object JournalEventWatchTest
{
  private val MyEvents1 =
    Stamped(110, "A1" <-: A1) ::
    Stamped(120, "A2" <-: A2) ::
    Nil

  private val MyEvents2 =
    Stamped(210, "B1" <-: B1) ::
    Stamped(220, "B2" <-: B2) ::
    Nil

  private sealed trait MyEvent extends Event

  private trait AEvent extends MyEvent {
    type Key = String
  }

  private case object A1 extends AEvent
  private case object A2 extends AEvent

  private trait BEvent extends MyEvent {
    type Key = String
  }

  private case object B1 extends BEvent
  private case object B2 extends BEvent

  private implicit val A1Codec = CirceUtils.singletonCodec(A1)
  private implicit val A2Codec = CirceUtils.singletonCodec(A2)
  private implicit val B1Codec = CirceUtils.singletonCodec(B1)
  private implicit val B2Codec = CirceUtils.singletonCodec(B2)

  implicit private val keyedEventTypedJsonCodec = KeyedEventTypedJsonCodec[MyEvent](
    KeyedSubtype.singleEvent[A1.type],
    KeyedSubtype.singleEvent[A2.type],
    KeyedSubtype.singleEvent[B1.type],
    KeyedSubtype.singleEvent[B2.type])
}
