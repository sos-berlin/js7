package js7.journal.watch

import io.circe.*
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.*
import izumi.reflect.Tag
import java.nio.file.Files
import java.util.UUID
import js7.base.BuildInfo
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.PositionAnd
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, EventRequest, EventSeq, JournalEvent, JournalHeader, JournalHeaders, JournalId, JournalPosition, JournalSeparators, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotableState, Stamped, TearableEventSeq}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.watch.JournalEventWatchTest.*
import js7.journal.watch.TestData.{writeJournal, writeJournalSnapshot}
import js7.journal.write.EventJournalWriter
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class JournalEventWatchTest extends OurTestSuite with BeforeAndAfterAll
{
  import TestState.keyedEventJsonCodec

  "JournalId is checked" in {
    withJournalMeta { journalMeta =>
      val myJournalId = JournalId.random()
      writeJournal(journalMeta, EventId.BeforeFirst, MyEvents1, journalId = myJournalId)
      val exception = intercept[IllegalArgumentException] {
        withJournal(journalMeta, MyEvents1.last.eventId) { (_, eventWatch) =>
        }
      }
      assert(exception.getMessage startsWith "requirement failed: JournalId ")
    }
  }

  "eventWatch.when" in {
    withJournalMeta { journalMeta =>
      writeJournal(journalMeta, EventId.BeforeFirst, MyEvents1)
      withJournal(journalMeta, MyEvents1.last.eventId) { (writer, eventWatch) =>
        def when(after: EventId) =
          eventWatch.when(EventRequest.singleClass[MyEvent](after = after, timeout = Some(30.s))).await(99.s).strict
        def observeFile(journalPosition: JournalPosition): List[Json] =
          eventWatch.observeFile(journalPosition, timeout = 0.s)
            .await(99.s)
            .orThrow
            .map(o => o.value.utf8String.parseJson.orThrow)
            .tail
            .toListL
            .await(99.s)

        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))
        assert(observeFile(JournalPosition(MyEvents1.last.eventId, 0L)) ==
          JournalSeparators.EventHeader :: Nil)

        writer.writeEvents(MyEvents2)
        assert(observeFile(JournalPosition(MyEvents1.last.eventId, 0L)) ==
          JournalSeparators.EventHeader :: Nil)
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))

        writer.flush(sync = false)
        assert(observeFile(JournalPosition(MyEvents1.last.eventId, 0L)) ==
          JournalSeparators.EventHeader :: MyEvents2.map(_.asJson))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))

        writer.onCommitted(writer.fileLengthAndEventId, MyEvents2.length)
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))
        assert(when(110L) == EventSeq.NonEmpty(MyEvents1.tail ++ MyEvents2))
        assert(when(120L) == EventSeq.NonEmpty(MyEvents2))
        assert(when(210L) == EventSeq.NonEmpty(MyEvents2.tail))
        assert(eventWatch
          .when(EventRequest.singleClass[MyEvent](after = 220L, timeout = Some(10.ms)))
          .await(99.s) == EventSeq.Empty(220L))

        eventWatch.releaseEvents(untilEventId = 0L)
        assert(journalMeta.listJournalFiles.map(_.file)
          == Vector(journalMeta.file(0L), journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))

        eventWatch.releaseEvents(untilEventId = 110L)
        assert(journalMeta.listJournalFiles.map(_.file)
          == Vector(journalMeta.file(0L), journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))

        eventWatch.releaseEvents(untilEventId = 120L)
        assert(journalMeta.listJournalFiles.map(_.file)
          == Vector(journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(120L))

        eventWatch.releaseEvents(untilEventId = 220L)
        assert(journalMeta.listJournalFiles.map(_.file)
          == Vector(journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(120L))
      }
    }
  }

  "CurrentEventReader only" - {
    "eventWatch.when" in {
      withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) =>
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst).get.strict.isEmpty)

        val events = Stamped(1L, "1" <-: A1) :: Stamped(2L, "2" <-: A1) :: Nil
        writer.writeEvents(events)
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst).get.strict.isEmpty)  // Not flushed, so nothing has been read

        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, events.length)
        val stampedEventSeq = eventWatch.eventsAfter(after = EventId.BeforeFirst).get.strict
        assert(stampedEventSeq == events)
        assert(eventWatch.eventsAfter(after = stampedEventSeq(0).eventId).get.strict == events.tail)
        assert(eventWatch.eventsAfter(after = stampedEventSeq(1).eventId).get.strict.isEmpty)

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](timeout = Some(30.s)))
          .await(99.s).strict == EventSeq.NonEmpty(events))
      }
    }

    "eventWatch.when with torn event stream" in {
      withJournalEventWatch(lastEventId = EventId(1000)) { (writer, eventWatch) =>
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst) == None)
        assert(eventWatch.eventsAfter(after = 999L) == None)
        assert(eventWatch.eventsAfter(after = 1000L).map(_.toList) == Some(Nil))

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](timeout = Some(30.s)))
          .await(99.s).strict == TearableEventSeq.Torn(after = 1000L))

        val anyFuture = eventWatch
          .when(EventRequest.singleClass[MyEvent](after = 1000L, timeout = Some(30.s)))
          .runToFuture
        writer.writeEvents(Stamped(1001L, "1" <-: A1) :: Nil)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, 1)
        val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
        assert(anyEvents == Stamped(1001L, "1" <-: A1) :: Nil)
      }
    }

    "eventWatch.when for selected event subclasses" in {
      withJournalEventWatch(lastEventId = EventId(1000)) { (writer, eventWatch) =>
        val anyFuture = eventWatch
          .when(EventRequest.singleClass[MyEvent](after = 1000L, timeout = Some(30.s)))
          .runToFuture
        val bFuture = eventWatch
          .when(EventRequest.singleClass[BEvent](after = 1000L, timeout = Some(30.s)))
          .runToFuture

        writer.writeEvents(Stamped(1001L, "1" <-: A1) :: Nil)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, 1)
        val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
        assert(anyEvents == Stamped(1001L, "1" <-: A1) :: Nil)

        writer.writeEvents(Stamped(1002L, "2" <-: B1) :: Nil)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, 1)
        val EventSeq.NonEmpty(bEventsIterator) = bFuture.await(99.s).strict
        assert(bEventsIterator == Stamped(1002L, "2" <-: B1) :: Nil)

        assert(eventWatch
          .when(EventRequest.singleClass[MyEvent](after = 1000L, timeout = Some(30.s)))
          .await(99.s).strict
          == EventSeq.NonEmpty(Stamped(1001L, "1" <-: A1) :: Stamped(1002L, "2" <-: B1) :: Nil))
      }
    }

    "eventWatch.whenKey, whenKeyedEvent" in {
      withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) =>
        val stampedSeq =
          Stamped(1L, "1" <-: A1) ::
          Stamped(2L, "1" <-: B1) ::
          Stamped(3L, "1" <-: A2) ::
          Stamped(4L, "2" <-: A2) ::
          Stamped(5L, "1" <-: B2) :: Nil
        writer.writeEvents(stampedSeq)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, stampedSeq.length)

        def eventsForKey[E <: MyEvent: ClassTag: Tag](key: E#Key) = {
          val EventSeq.NonEmpty(eventIterator) = eventWatch
            .whenKey[E](EventRequest.singleClass(timeout = Some(99.s)), key)
            .await(10.s).strict
          eventIterator.toVector.map(_.value)
        }
        assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
        assert(eventsForKey[AEvent]("2") == Vector(A2))
        assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

        def keyedEvent[E <: MyEvent: ClassTag: Tag](key: E#Key) =
          eventWatch.whenKeyedEvent[E](EventRequest.singleClass(timeout = Some(99.s)), key) await 10.s
        assert(keyedEvent[AEvent]("1") == A1)
        assert(keyedEvent[AEvent]("2") == A2)
        assert(keyedEvent[BEvent]("1") == B1)
      }
    }

    "Second onJournalingStarted (snapshot)" in {
      withJournalMeta { journalMeta =>
        autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
          autoClosing(EventJournalWriter.forTest(journalMeta, after = EventId.BeforeFirst, journalId, Some(eventWatch))) { writer =>
            writer.beginEventSection(sync = false)
            writer.onJournalingStarted()
            val stampedSeq =
              Stamped(1L, "1" <-: A1) ::
              Stamped(2L, "1" <-: B1) ::
              Stamped(3L, "1" <-: A2) :: Nil
            writer.writeEvents(stampedSeq)
            writer.flush(sync = false)
            writer.onCommitted(writer.fileLengthAndEventId, stampedSeq.length)
            writer.endEventSection(sync = false)
          }
          assert(eventWatch.fileEventIds == EventId.BeforeFirst :: Nil)

          autoClosing(EventJournalWriter.forTest(journalMeta, after = 3L, journalId, Some(eventWatch))) { writer =>
            writer.beginEventSection(sync = false)
            writer.onJournalingStarted()
            val stampedSeq =
              Stamped(4L, "2" <-: A2) ::
              Stamped(5L, "1" <-: B2) :: Nil
            writer.writeEvents(stampedSeq)
            writer.flush(sync = false)
            writer.onCommitted(writer.fileLengthAndEventId, stampedSeq.length)
            writer.endEventSection(sync = false)
          }
          assert(eventWatch.fileEventIds == EventId.BeforeFirst :: 3 :: Nil)
        }
      }
    }

    "observe" in {
      withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) =>
        val stampeds = mutable.Buffer[Stamped[KeyedEvent[AEvent]]]()
        val observed = eventWatch.observe(EventRequest.singleClass[AEvent](limit = 3, timeout = Some(99.s)))
          .foreach(stampeds += _)
        assert(stampeds.isEmpty)

        val stampedSeq = Seq(Stamped(1L, "1" <-: A1), Stamped(2L, "2" <-: B1), Stamped(3L, "3" <-: A1))
        writer.writeEvents(stampedSeq)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, stampedSeq.length)
        waitForCondition(99.s, 10.ms) { stampeds.size == 2 }
        assert(stampeds == Seq(Stamped(1L, "1" <-: A1), Stamped(3L, "3" <-: A1)))

        writer.writeEvents(Seq(Stamped(4L, "4" <-: A1)))
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, 1)
        waitForCondition(99.s, 10.ms) { stampeds.size == 3 }
        assert(stampeds == Seq(Stamped(1L, "1" <-: A1), Stamped(3L, "3" <-: A1), Stamped(4L, "4" <-: A1)))

        // limit=3 reached
        observed await 99.s
      }
    }

    "observe Torn" in {
      // Wie geben wir am besten 'Torn' zurück? Als Ende des Streams, als Exception oder als eigenes Objekt?
      withJournalEventWatch(lastEventId = EventId(1000)) { (_, eventWatch) =>
        val e = intercept[TornException] {
          eventWatch
            .observe(EventRequest.singleClass[AEvent](after = EventId(10), timeout = Some(99.s)))
            .countL
            .await(99.s)
        }
        assert(e.after == 10 && e.tornEventId == 1000)
      }
    }

    "observe after=(unknown EventId)" in {
      withJournalMeta { journalMeta =>
        withJournal(journalMeta, lastEventId = EventId(100)) { (writer, eventWatch) =>
          writer.writeEvents(MyEvents1)
          writer.flush(sync = false)
          writer.onCommitted(writer.fileLengthAndEventId, MyEvents1.length)

          val e = intercept[TornException] {
            eventWatch.observe(EventRequest
              .singleClass[AEvent](after = EventId(115), timeout = Some(99.s)))
              .countL.await(99.s)
          }
          assert(e.after == 115 && e.tornEventId == 100)

          val stampeds = mutable.Buffer[Stamped[KeyedEvent[AEvent]]]()
          eventWatch
            .observe(EventRequest.singleClass[AEvent](after = EventId(110), timeout = Some(500.ms)))
            .foreach(stampeds += _)
            .await(99.s)
          assert(stampeds == MyEvents1(1) :: Nil)
        }
      }
    }
  }

  "snapshotAfter" in {
    withJournalMeta { journalMeta =>
      autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
        // --0.journal with no snapshot objects
        writeJournalSnapshot(journalMeta, after = EventId.BeforeFirst, Nil)
        autoClosing(EventJournalWriter.forTest(journalMeta, after = EventId.BeforeFirst, journalId, Some(eventWatch), withoutSnapshots = false)) { writer =>
          writer.onJournalingStarted()  // Notifies eventWatch about this journal file

          val Some(observable) = eventWatch.snapshotAfter(EventId.BeforeFirst)
          // Contains only JournalHeader
          assert(observable.toListL.await(99.s).map(_.asInstanceOf[JournalHeader].eventId) == List(EventId.BeforeFirst))
        }
      }

      // --100.journal with some snapshot objects
      val snapshotObjects = List[Any](ASnapshot("ONE"), ASnapshot("TWO"))
      val after = EventId(100)
      val file = writeJournalSnapshot(journalMeta, after = after, snapshotObjects)
      autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
        val lengthAndEventId = PositionAnd(Files.size(file), after)
        eventWatch.onJournalingStarted(journalMeta.file(after), journalId,
          lengthAndEventId, lengthAndEventId, isActiveNode = true)
        locally {
          val Some(observable) = eventWatch.snapshotAfter(EventId.BeforeFirst)
          // Contains only JournalHeader
          assert(observable.toListL.await(99.s)
            .map(_.asInstanceOf[JournalHeader].eventId)
            == List(EventId.BeforeFirst))
        }
        locally {
          val Some(observable) = eventWatch.snapshotAfter(99L)
          // Contains only JournalHeader
          assert(observable.toListL.await(99.s)
            .map(_.asInstanceOf[JournalHeader].eventId)
            == List(EventId.BeforeFirst))
        }
        locally {
          val Some(observable) = eventWatch.snapshotAfter(100L)
          assert(observable.map {
            case o: JournalHeader => o.eventId
            case o => o
          }.toListL.await(99.s) == 100L :: snapshotObjects)
        }
        locally {
          val Some(observable) = eventWatch.snapshotAfter(101L)
          assert(observable.map {
            case o: JournalHeader => o.eventId
            case o => o
          }.toListL.await(99.s) == 100L :: snapshotObjects)
        }
      }
    }
  }

  "Read/write synchronization crash test" in {
    pending   // TODO Intensiv schreiben und lesen, um Synchronisation zu prüfen
    // Mit TakeSnapshot prüfen
  }

  "observeFile" in {
    withJournalEventWatch(lastEventId = EventId.BeforeFirst) { (writer, eventWatch) =>
      assert(eventWatch.observeFile(JournalPosition(123L, 0), timeout = 99.s).await(99.s)
        == Left(Problem("Unknown journal file=123")))

      val jsons = mutable.Buffer[Json]()
      val observing = eventWatch.observeFile(JournalPosition(EventId.BeforeFirst, 0), timeout = 99.s)
        .await(99.s)
        .orThrow
        .onErrorRecoverWith {
          case _: EventReader.TimeoutException => Observable.empty
        }
        .foreach(o => jsons += o.value.utf8String.parseJsonOrThrow)
      waitForCondition(99.s, 10.ms) { jsons.size == 2 }
      assert(jsons(0).as[JournalHeader].orThrow.js7Version == BuildInfo.longVersion)
      assert(jsons(1) == JournalSeparators.EventHeader)

      writer.writeEvents(Stamped(1L, "1" <-: A1) :: Stamped(2L, "2" <-: B1) :: Nil)
      writer.flush(sync = false)  // TODO Flush sollte hier nicht erforderlich sein!
      writer.onCommitted(writer.fileLengthAndEventId, n = 2)
      waitForCondition(99.s, 10.ms) { jsons.size == 4 }
      assert(jsons(2).as[Stamped[KeyedEvent[Event]]] == Right(Stamped(1L, "1" <-: A1)))
      assert(jsons(3).as[Stamped[KeyedEvent[Event]]] == Right(Stamped(2L, "2" <-: B1)))

      writer.writeEvents(Stamped(3L, "3" <-: A1) :: Nil)
      writer.flush(sync = false)  // TODO Flush sollte hier nicht erforderlich sein!
      writer.onCommitted(writer.fileLengthAndEventId, 1)
      waitForCondition(99.s, 10.ms) { jsons.size == 5 }
      assert(jsons(4).as[Stamped[KeyedEvent[Event]]] == Right(Stamped(3L, "3" <-: A1)))

      observing.cancel()
      if (isWindows) sleep(100.ms)  // Let observing close the read file to allow deletion
    }
  }

  private def withJournalEventWatch(lastEventId: EventId)
    (body: (EventJournalWriter, JournalEventWatch) => Unit)
  : Unit =
    withJournalMeta { journalMeta =>
      withJournal(journalMeta, lastEventId)(body)
    }

  private def withJournalMeta(body: JournalMeta => Unit): Unit =
    withTemporaryDirectory("JournalEventWatchTest") { directory =>
      val journalMeta = JournalMeta(TestState, directory / "test")
      body(journalMeta)
    }

  private def withJournal(journalMeta: JournalMeta, lastEventId: EventId)
    (body: (EventJournalWriter, JournalEventWatch) => Unit)
  : Unit =
    autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
      autoClosing(EventJournalWriter.forTest(journalMeta, after = lastEventId, journalId, Some(eventWatch))) { writer =>
        writer.writeHeader(JournalHeaders.forTest(TestState.name, journalId, eventId = lastEventId))
        writer.beginEventSection(sync = false)
        writer.onJournalingStarted()
        body(writer, eventWatch)
        writer.endEventSection(sync = false)
      }
    }
}

private object JournalEventWatchTest
{
  private val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))

  private val MyEvents1: List[Stamped[KeyedEvent[Event]]] =
    Stamped(110L, "A1" <-: A1) ::
    Stamped(120L, "A2" <-: A2) ::
    Nil

  private val MyEvents2: List[Stamped[KeyedEvent[Event]]] =
    Stamped(210L, "B1" <-: B1) ::
    Stamped(220L, "B2" <-: B2) ::
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

  private implicit val A1Codec: Codec.AsObject[A1.type] = CirceUtils.singletonCodec(A1)
  private implicit val A2Codec: Codec.AsObject[A2.type] = CirceUtils.singletonCodec(A2)
  private implicit val B1Codec: Codec.AsObject[B1.type] = CirceUtils.singletonCodec(B1)
  private implicit val B2Codec: Codec.AsObject[B2.type] = CirceUtils.singletonCodec(B2)


  private case class ASnapshot(string: String)

  private object TestState extends SnapshotableState.HasCodec {
    val name = "TestState"
    implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
      KeyedEventTypedJsonCodec(
        KeyedSubtype[JournalEvent],
        KeyedSubtype.singleEvent[A1.type],
        KeyedSubtype.singleEvent[A2.type],
        KeyedSubtype.singleEvent[B1.type],
        KeyedSubtype.singleEvent[B2.type])

    def snapshotObjectJsonCodec: TypedJsonCodec[Any] = TypedJsonCodec[Any](
      Subtype[JournalHeader],
      Subtype(deriveCodec[ASnapshot]))
  }
}
