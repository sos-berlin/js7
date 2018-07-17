package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.event.RealEventReader
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.SetOnce
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.CurrentJournalEventReaderTest._
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import java.nio.file.Path
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class CurrentJournalEventReaderTest extends FreeSpec with BeforeAndAfterAll {

  import CurrentJournalEventReaderTest.keyedEventTypedJsonCodec

  private implicit lazy val timerService = TimerService(idleTimeout = Some(1.s))

  "eventReader.when" in {
    withJournal(lastEventId = EventId.BeforeFirst) { (writer, eventReader) ⇒
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst).get.strict.isEmpty)

      val events = Stamped(1, "1" <-: A1) :: Stamped(2, "2" <-: A1) :: Nil
      writer.writeEvents(events)
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst).get.strict.isEmpty)  // Not flushed, so nothing has been read

      writer.flush()
      val stampedEventSeq = eventReader.eventsAfter(after = EventId.BeforeFirst).get.strict
      assert(stampedEventSeq == events)
      assert(eventReader.eventsAfter(after = stampedEventSeq(0).eventId).get.strict == events.tail)
      assert(eventReader.eventsAfter(after = stampedEventSeq(1).eventId).get.strict.isEmpty)

      assert(eventReader.when(EventRequest.singleClass[MyEvent](after = EventId.BeforeFirst, timeout = 30.seconds)).await(99.s).strict ==
        EventSeq.NonEmpty(events))
    }
  }

  "eventReader.when with torn event stream" in {
    withJournal(lastEventId = EventId(1000)) { (writer, eventReader) ⇒
      assert(eventReader.eventsAfter(after = EventId.BeforeFirst) == None)
      assert(eventReader.eventsAfter(after = 999) == None)
      assert(eventReader.eventsAfter(after = 1000).map(_.toVector) == Some(Nil))

      assert(eventReader.when(EventRequest.singleClass[MyEvent](after = EventId.BeforeFirst, timeout = 30.seconds)).await(99.s).strict ==
        TearableEventSeq.Torn(after = 1000))

      val anyFuture = eventReader.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.seconds)).runAsync
      writer.writeEvents(Stamped(1001, "1" <-: A1) :: Nil)
      writer.flush()
      val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
      assert(anyEvents == Stamped(1001, "1" <-: A1) :: Nil)
    }
  }

  "eventReader.when for selected event subclasses" in {
    withJournal(lastEventId = EventId(1000)) { (writer, eventReader) ⇒
      val anyFuture = eventReader.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.seconds)).runAsync
      val bFuture = eventReader.when(EventRequest.singleClass[BEvent](after = 1000, timeout = 30.seconds)).runAsync

      writer.writeEvents(Stamped(1001, "1" <-: A1) :: Nil)
      writer.flush()
      val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
      assert(anyEvents == Stamped(1001, "1" <-: A1) :: Nil)

      writer.writeEvents(Stamped(1002, "2" <-: B1) :: Nil)
      writer.flush()
      val EventSeq.NonEmpty(bEventsIterator) = bFuture.await(99.s).strict
      assert(bEventsIterator == Stamped(1002, "2" <-: B1) :: Nil)

      assert(eventReader.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = 30.seconds)).await(99.s).strict ==
        EventSeq.NonEmpty(Stamped(1001, "1" <-: A1) :: Stamped(1002, "2" <-: B1) :: Nil))
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
        val EventSeq.NonEmpty(eventIterator) = eventReader.whenKey[E](EventRequest.singleClass(after = EventId.BeforeFirst, 99.seconds), key).await(10.s).strict
        eventIterator.toVector map { _.value }
      }
      assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
      assert(eventsForKey[AEvent]("2") == Vector(A2))
      assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

      def keyedEvent[E <: MyEvent: ClassTag](key: E#Key) =
        eventReader.whenKeyedEvent[E](EventRequest.singleClass(after = EventId.BeforeFirst, 99.seconds), key) await 10.s
      assert(keyedEvent[AEvent]("1") == A1)
      assert(keyedEvent[AEvent]("2") == A2)
      assert(keyedEvent[BEvent]("1") == B1)
    }
  }

  "observe" in {
    withJournal(lastEventId = EventId.BeforeFirst) { (writer, eventReader) ⇒
      val stampeds = mutable.Buffer[Stamped[KeyedEvent[AEvent]]]()
      val observed = eventReader.observe(EventRequest.singleClass[AEvent](after = EventId.BeforeFirst, limit = 3, timeout = 99.seconds))
        .foreach(stampeds.+=)
      assert(stampeds.isEmpty)

      writer.writeEvents(Stamped(1, "1" <-: A1) :: Stamped(2, "2" <-: B1) :: Stamped(3, "3" <-: A1) :: Nil)
      writer.flush()
      waitForCondition(99.s, 10.ms) { stampeds.size == 2 }
      assert(stampeds == Stamped(1, "1" <-: A1) :: Stamped(3, "3" <-: A1) :: Nil)

      writer.writeEvents(Stamped(4, "4" <-: A1) :: Nil)
      writer.flush()
      waitForCondition(99.s, 10.ms) { stampeds.size == 3 }
      assert(stampeds == Stamped(1, "1" <-: A1) :: Stamped(3, "3" <-: A1) :: Stamped(4, "4" <-: A1) :: Nil)

      // limit=3 reached
      observed await 99.s
    }
  }

  "observe Torn" in {
    // Wie geben wir am besten 'Torn' zurück? Als Ende des Streams, als Exception oder als eigenes Objekt?
    withJournal(lastEventId = EventId(1000)) { (_, eventReader) ⇒
      val e = intercept[RealEventReader.TornException] {
        eventReader.observe(EventRequest.singleClass[AEvent](after = 10, timeout = 99.seconds)).countL await 9.s
      }
      assert(e.after == 10 && e.tornEventId == 1000)
    }
  }

  private def withJournal(lastEventId: EventId)(body: (JournalWriter[MyEvent], CurrentJournalEventReader[MyEvent]) ⇒ Unit): Unit =
    withTemporaryDirectory("CurrentJournalEventReaderTest") { directory ⇒
      val journalMeta = JournalMeta(
        TypedJsonCodec(),  // TODO Test with snapshots
        keyedEventTypedJsonCodec,
        directory / "test")
      val journalFile = journalMeta.file(after = lastEventId)
      var eventReader = new SetOnce[CurrentJournalEventReader[MyEvent]]
      val writer = new JournalWriter[MyEvent](journalMeta, after = lastEventId,
        Some(new WriterReaderAdapter {
          def onJournalingStarted(file: Path, flushedLengthAndEventId: PositionAnd[EventId]) = {
            assert(file == journalFile)
            assert(flushedLengthAndEventId.value == lastEventId)
            eventReader := new CurrentJournalEventReader[MyEvent](journalMeta, file, flushedLengthAndEventId)
          }
          def onEventsAdded(flushedPositionAndEventId: PositionAnd[EventId]) = eventReader().onEventsAdded(flushedPositionAndEventId)
        }))
      writer.beginSnapshotSection()
      writer.beginEventSection()
      val reader = new JournalReader[MyEvent](journalMeta, journalFile)
      assert(reader.recoverNext().isEmpty)
      body(writer, eventReader())
      reader.close()
      writer.close()
    }
}

private object CurrentJournalEventReaderTest
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

  implicit private val keyedEventTypedJsonCodec = KeyedEventTypedJsonCodec[MyEvent](
    KeyedSubtype.singleEvent[A1.type],
    KeyedSubtype.singleEvent[A2.type],
    KeyedSubtype.singleEvent[B1.type],
    KeyedSubtype.singleEvent[B2.type])
}
