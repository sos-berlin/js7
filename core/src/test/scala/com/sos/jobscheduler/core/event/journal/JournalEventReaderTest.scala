package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.JournalEventReaderTest._
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class JournalEventReaderTest extends FreeSpec with BeforeAndAfterAll {

  private implicit val eventJsonCodec = TestJournalMeta.eventJsonCodec
  private implicit lazy val timerService = TimerService(idleTimeout = Some(1.s))

  "eventReader.keyedEventQueue.after" in {
    withJournal(lastEventId = EventId.BeforeFirst) { (writer, eventReader) ⇒
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst).await(99.s).get.isEmpty)

      writer.writeEvents(Stamped(1, "1" <-: A1) :: Stamped(2, "2" <-: A1) :: Nil)
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst).await(99.s).get.isEmpty)  // Not flushed, so nothing has been read

      writer.flush()
      val stampedEventSeq = eventReader.eventsAfter(after = EventId.BeforeFirst).await(99.s).get.toList
      assert(stampedEventSeq == Stamped(1, "1" <-: A1) :: Stamped(2, "2" <-: A1) :: Nil)
      assert(eventReader.eventsAfter(after = stampedEventSeq(0).eventId).await(99.s).get.toList == Stamped(2, "2" <-: A1) :: Nil)
      assert(eventReader.eventsAfter(after = stampedEventSeq(1).eventId).await(99.s).get.isEmpty)
    }
  }

  "eventReader.when with torn event stream" in {
    withJournal(lastEventId = EventId(1000)) { (writer, eventReader) ⇒
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst).await(99.s) == None)
      assert(eventReader.when(EventRequest.singleClass[MyEvent](after = EventId.BeforeFirst, timeout = 30.s)).await(99.s) ==
        TearableEventSeq.Torn(oldestKnownEventId = 1000))

      val anyFuture = eventReader.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.s)).runAsync
      writer.writeEvents(Stamped(1001, "1" <-: A1) :: Nil)
      writer.flush()
      val EventSeq.NonEmpty(anyEvents) = anyFuture await 99.s
      assert(anyEvents.toList == Stamped(1001, "1" <-: A1) :: Nil)
    }
  }

  "eventReader.when for selected event subclasses" in {
    withJournal(lastEventId = EventId(1000)) { (writer, eventReader) ⇒
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst).await(99.s) == None)
      assert(eventReader.when(EventRequest.singleClass[MyEvent](after = EventId.BeforeFirst, timeout = 30.s)).await(99.s) ==
        TearableEventSeq.Torn(oldestKnownEventId = 1000))

      val anyFuture = eventReader.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.s)).runAsync
      val bFuture = eventReader.when(EventRequest.singleClass[BEvent](after = 1000, timeout = 30.s)).runAsync

      writer.writeEvents(Stamped(1001, "1" <-: A1) :: Nil)
      writer.flush()
      val EventSeq.NonEmpty(anyEvents) = anyFuture await 99.s
      assert(anyEvents.toList == Stamped(1001, "1" <-: A1) :: Nil)

      writer.writeEvents(Stamped(1002, "2" <-: B1) :: Nil)
      writer.flush()
      val EventSeq.NonEmpty(bEventsIterator) = bFuture await 99.s
      assert(bEventsIterator.toList == Stamped(1002, "2" <-: B1) :: Nil)
    }
  }

  "eventReader.whenKey, whenKeyedEvent" in {
    withJournal(lastEventId = EventId.BeforeFirst) { (writer, eventReader) ⇒
      writer.writeEvents(
        Stamped(1, "1" <-: A1) ::
        Stamped(2, "1" <-: B1) ::
        Stamped(3, "1" <-: A2) ::
        Stamped(4, "2" <-: A2) ::
        Stamped(5, "1" <-: B2) :: Nil)
      writer.flush()

      def eventsForKey[E <: MyEvent: ClassTag](key: E#Key) = {
        val EventSeq.NonEmpty(eventIterator) = eventReader.whenKey[E](EventRequest.singleClass(after = EventId.BeforeFirst, 99.s), key) await 10.s
        eventIterator.toVector map { _.value }
      }
      assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
      assert(eventsForKey[AEvent]("2") == Vector(A2))
      assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

      def keyedEvent[E <: MyEvent: ClassTag](key: E#Key) =
        eventReader.whenKeyedEvent[E](EventRequest.singleClass(after = EventId.BeforeFirst, 99.s), key) await 10.s
      assert(keyedEvent[AEvent]("1") == A1)
      assert(keyedEvent[AEvent]("2") == A2)
      assert(keyedEvent[BEvent]("1") == B1)
    }
  }

  "Read/write synchronization crash test" in {
    pending   // TODO Intensiv schreiben und lesen, um Synchronisation zu prüfen
  }

  private def withJournal(lastEventId: EventId)(body: (JournalWriter[MyEvent], JournalEventReaderProvider[MyEvent]) ⇒ Unit): Unit =
    withTemporaryFile { journalFile ⇒
      val eventReaderProvider = new JournalEventReaderProvider[MyEvent](TestJournalMeta, journalFile, 99.s)
      val writer = new JournalWriter[MyEvent](TestJournalMeta, journalFile, eventReaderProvider = Some(eventReaderProvider))
      writer.startSnapshots(lastEventId = lastEventId)
      writer.startEvents()
      val reader = new JournalReader[MyEvent](TestJournalMeta, journalFile)
      assert(reader.recoverNext().isEmpty)
      body(writer, eventReaderProvider)
      reader.close()
      writer.close()
    }
}

private object JournalEventReaderTest
{
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

  private val TestJournalMeta = new JournalMeta[MyEvent](
    TypedJsonCodec(),  // TODO Test with snapshots
    KeyedEventTypedJsonCodec[MyEvent](
      KeyedSubtype.singleEvent[A1.type],
      KeyedSubtype.singleEvent[A2.type],
      KeyedSubtype.singleEvent[B1.type],
      KeyedSubtype.singleEvent[B2.type]))
}
