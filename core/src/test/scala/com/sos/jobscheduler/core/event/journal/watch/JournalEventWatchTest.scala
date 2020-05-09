package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.BuildInfo
import com.sos.jobscheduler.base.circeutils.CirceUtils
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.common.event.{PositionAnd, TornException}
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax.RichPath
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatchTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.{writeJournal, writeJournalSnapshot}
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter
import com.sos.jobscheduler.data.event.JournalHeader.JournalIdMismatchProblem
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, JournalEvent, JournalHeader, JournalId, JournalSeparators, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import io.circe._
import io.circe.syntax._
import java.nio.file.Files
import java.util.UUID
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
final class JournalEventWatchTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private implicit val keyedEventTypedJsonCodec = JournalEventWatchTest.keyedEventTypedJsonCodec

  "JournalId is checked" in {
    withJournalMeta { journalMeta =>
      val myJournalId = JournalId(UUID.randomUUID)
      val file = writeJournal(journalMeta, EventId.BeforeFirst, MyEvents1, journalId = myJournalId)
      withJournal(journalMeta, MyEvents1.last.eventId) { (_, eventWatch) =>
        intercept[ProblemException] {
          // Read something to open the first journal file and to check its header
          eventWatch.snapshotObjectsFor(EventId.BeforeFirst)
        } .problem == JournalIdMismatchProblem(file, expectedJournalId = myJournalId, foundJournalId = journalId)
      }
    }
  }

  "eventWatch.when" in {
    withJournalMeta { journalMeta =>
      writeJournal(journalMeta, EventId.BeforeFirst, MyEvents1)
      withJournal(journalMeta, MyEvents1.last.eventId) { (writer, eventWatch) =>
        def when(after: EventId) =
          eventWatch.when(EventRequest.singleClass[MyEvent](after = after, timeout = Some(30.s))).await(99.s).strict
        def observeFile(fileEventId: EventId, position: Long): List[Json] =
          eventWatch.observeFile(Some(fileEventId), position = Some(position), timeout = 0.s)
            .orThrow
            .map(o => o.value.utf8String.parseJsonChecked.orThrow)
            .tail
            .toListL
            .await(99.s)

        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))
        assert(observeFile(MyEvents1.last.eventId, position = 0L) == JournalSeparators.EventHeader :: Nil)

        writer.writeEvents(MyEvents2)
        assert(observeFile(MyEvents1.last.eventId, position = 0L) == JournalSeparators.EventHeader :: Nil)
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))

        writer.flush(sync = false)
        assert(observeFile(MyEvents1.last.eventId, position = 0L) == JournalSeparators.EventHeader :: MyEvents2.map(_.asJson))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1))

        writer.onCommitted(writer.fileLengthAndEventId, MyEvents2.length)
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))
        assert(when(110L) == EventSeq.NonEmpty(MyEvents1.tail ++ MyEvents2))
        assert(when(120L) == EventSeq.NonEmpty(MyEvents2))
        assert(when(210L) == EventSeq.NonEmpty(MyEvents2.tail))
        assert(eventWatch.when(EventRequest.singleClass[MyEvent](after = 220L, timeout = Some(10.ms))).await(99.s).strict == EventSeq.Empty(220L))

        eventWatch.releaseEvents(untilEventId = 0L)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(0L), journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))

        eventWatch.releaseEvents(untilEventId = 110L)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(0L), journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == EventSeq.NonEmpty(MyEvents1 ++ MyEvents2))

        eventWatch.releaseEvents(untilEventId = 120L)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(120L)))
        assert(when(EventId.BeforeFirst) == TearableEventSeq.Torn(120L))

        eventWatch.releaseEvents(untilEventId = 220L)
        assert(JournalFiles.listJournalFiles(journalFileBase = journalMeta.fileBase).map(_.file) == Vector(journalMeta.file(120L)))
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

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](timeout = Some(30.s))).await(99.s).strict ==
          EventSeq.NonEmpty(events))
      }
    }

    "eventWatch.when with torn event stream" in {
      withJournalEventWatch(lastEventId = EventId(1000)) { (writer, eventWatch) =>
        assert(eventWatch.eventsAfter(after = EventId.BeforeFirst) == None)
        assert(eventWatch.eventsAfter(after = 999L) == None)
        assert(eventWatch.eventsAfter(after = 1000L).map(_.toVector) == Some(Nil))

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](timeout = Some(30.s))).await(99.s).strict ==
          TearableEventSeq.Torn(after = 1000L))

        val anyFuture = eventWatch.when(EventRequest.singleClass[MyEvent](after = 1000L, timeout = Some(30.s))).runToFuture
        writer.writeEvents(Stamped(1001L, "1" <-: A1) :: Nil)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, 1)
        val EventSeq.NonEmpty(anyEvents) = anyFuture.await(99.s).strict
        assert(anyEvents == Stamped(1001L, "1" <-: A1) :: Nil)
      }
    }

    "eventWatch.when for selected event subclasses" in {
      withJournalEventWatch(lastEventId = EventId(1000)) { (writer, eventWatch) =>
        val anyFuture = eventWatch.when(EventRequest.singleClass[MyEvent](after = 1000L, timeout = Some(30.s))).runToFuture
        val bFuture = eventWatch.when(EventRequest.singleClass[BEvent](after = 1000L, timeout = Some(30.s))).runToFuture

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

        assert(eventWatch.when(EventRequest.singleClass[MyEvent](after = 1000, timeout = Some(30.s))).await(99.s).strict ==
          EventSeq.NonEmpty(Stamped(1001L, "1" <-: A1) :: Stamped(1002L, "2" <-: B1) :: Nil))
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

        def eventsForKey[E <: MyEvent: ClassTag: TypeTag](key: E#Key) = {
          val EventSeq.NonEmpty(eventIterator) = eventWatch.whenKey[E](EventRequest.singleClass(timeout = Some(99.s)), key).await(10.s).strict
          eventIterator.toVector map { _.value }
        }
        assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
        assert(eventsForKey[AEvent]("2") == Vector(A2))
        assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

        def keyedEvent[E <: MyEvent: ClassTag: TypeTag](key: E#Key) =
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

          autoClosing(EventJournalWriter.forTest(journalMeta, after = 3, journalId, Some(eventWatch))) { writer =>
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

        val stampedSeq = Stamped(1L, "1" <-: A1) :: Stamped(2L, "2" <-: B1) :: Stamped(3L, "3" <-: A1) :: Nil
        writer.writeEvents(stampedSeq)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, stampedSeq.length)
        waitForCondition(99.s, 10.ms) { stampeds.size == 2 }
        assert(stampeds == Stamped(1L, "1" <-: A1) :: Stamped(3L, "3" <-: A1) :: Nil)

        writer.writeEvents(Stamped(4L, "4" <-: A1) :: Nil)
        writer.flush(sync = false)
        writer.onCommitted(writer.fileLengthAndEventId, 1)
        waitForCondition(99.s, 10.ms) { stampeds.size == 3 }
        assert(stampeds == Stamped(1L, "1" <-: A1) :: Stamped(3L, "3" <-: A1) :: Stamped(4L, "4" <-: A1) :: Nil)

        // limit=3 reached
        observed await 99.s
      }
    }

    "observe Torn" in {
      // Wie geben wir am besten 'Torn' zurück? Als Ende des Streams, als Exception oder als eigenes Objekt?
      withJournalEventWatch(lastEventId = EventId(1000)) { (_, eventWatch) =>
        val e = intercept[TornException] {
          eventWatch.observe(EventRequest.singleClass[AEvent](after = EventId(10), timeout = Some(99.s))).countL await 99.s
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
            eventWatch.observe(EventRequest.singleClass[AEvent](after = EventId(115), timeout = Some(99.s))).countL await 99.s
          }
          assert(e.after == 115 && e.tornEventId == 100)

          val stampeds = mutable.Buffer[Stamped[KeyedEvent[AEvent]]]()
          eventWatch.observe(EventRequest.singleClass[AEvent](after = EventId(110), timeout = Some(500.ms))).foreach(stampeds += _) await 99.s
          assert(stampeds == MyEvents1(1) :: Nil)
        }
      }
    }
  }

  "snapshotObjectsFor" in {
    withJournalMeta { journalMeta_ =>
      val journalMeta = journalMeta_.copy(snapshotJsonCodec = SnapshotJsonCodec)

      autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
        // --0.journal with no snapshot objects
        writeJournalSnapshot(journalMeta, after = EventId.BeforeFirst, Nil)
        autoClosing(EventJournalWriter.forTest(journalMeta, after = EventId.BeforeFirst, journalId, Some(eventWatch), withoutSnapshots = false)) { writer =>
          writer.onJournalingStarted()  // Notifies eventWatch about this journal file

          val Some((eventId, iterator)) = eventWatch.snapshotObjectsFor(EventId.BeforeFirst)
          assert(eventId == EventId.BeforeFirst)
          assert(iterator.toVector.map(_.isInstanceOf[JournalHeader]) == Vector(true))
          iterator.close()
        }
      }

      // --100.journal with some snapshot objects
      val snapshotObjects = List[Any](ASnapshot("ONE"), ASnapshot("TWO"))
      val after = EventId(100)
      val file = writeJournalSnapshot(journalMeta, after = after, snapshotObjects)
      autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
        val lengthAndEventId = PositionAnd(Files.size(file), after)
        eventWatch.onJournalingStarted(journalMeta.file(after), journalId, lengthAndEventId, lengthAndEventId)
        locally {
          val Some((eventId, iterator)) = eventWatch.snapshotObjectsFor(EventId.BeforeFirst)
          assert(eventId == EventId.BeforeFirst && iterator.toVector.map(_.isInstanceOf[JournalHeader]) == Vector(true))  // Contains only JournalHeader
          iterator.close()
        }
        locally {
          val Some((eventId, iterator)) = eventWatch.snapshotObjectsFor(99L)
          assert(eventId == EventId.BeforeFirst && iterator.toVector.map(_.isInstanceOf[JournalHeader]) == Vector(true))  // Contains only JournalHeader
          iterator.close()
        }
        locally {
          val Some((eventId, iterator)) = eventWatch.snapshotObjectsFor(100L)
          assert(eventId == 100 && iterator.filterNot(_.isInstanceOf[JournalHeader]).toList == snapshotObjects)
          iterator.close()
        }
        locally {
          val Some((eventId, iterator)) = eventWatch.snapshotObjectsFor(101L)
          assert(eventId == 100 && iterator.filterNot(_.isInstanceOf[JournalHeader]).toList == snapshotObjects)
          iterator.close()
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
      assert(eventWatch.observeFile(fileEventId = Some(123), position = Some(0), timeout = 99.s)
        == Left(Problem("Unknown journal file=123")))

      val jsons = mutable.Buffer[Json]()
      eventWatch.observeFile(fileEventId = Some(EventId.BeforeFirst), position = Some(0), timeout = 99.s)
        .orThrow
        .onErrorRecoverWith {
          case _: EventReader.TimeoutException => Observable.empty
        }
        .foreach(o => jsons += o.value.utf8String.parseJsonOrThrow)
      waitForCondition(99.s, 10.ms) { jsons.size == 2 }
      assert(jsons(0).as[JournalHeader].orThrow.softwareVersion == BuildInfo.version)
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
    }
  }

  private def withJournalEventWatch(lastEventId: EventId)(body: (EventJournalWriter, JournalEventWatch) => Unit): Unit = {
    withJournalMeta { journalMeta =>
      withJournal(journalMeta, lastEventId)(body)
    }
  }

  private def withJournalMeta(body: JournalMeta => Unit): Unit =
    withTemporaryDirectory("JournalEventWatchTest") { directory =>
      val journalMeta = JournalMeta(
        TypedJsonCodec(),  // No snapshots
        keyedEventTypedJsonCodec,
        directory / "test")
      body(journalMeta)
    }

  private def withJournal(journalMeta: JournalMeta, lastEventId: EventId)(body: (EventJournalWriter, JournalEventWatch) => Unit): Unit =
    autoClosing(new JournalEventWatch(journalMeta, JournalEventWatch.TestConfig)) { eventWatch =>
      autoClosing(EventJournalWriter.forTest(journalMeta, after = lastEventId, journalId, Some(eventWatch))) { writer =>
        writer.writeHeader(JournalHeader.forTest(journalId, eventId = lastEventId))
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

  private implicit val A1Codec = CirceUtils.singletonCodec(A1)
  private implicit val A2Codec = CirceUtils.singletonCodec(A2)
  private implicit val B1Codec = CirceUtils.singletonCodec(B1)
  private implicit val B2Codec = CirceUtils.singletonCodec(B2)

  private implicit val keyedEventTypedJsonCodec = KeyedEventTypedJsonCodec[Event](
    KeyedSubtype[JournalEvent],
    KeyedSubtype.singleEvent[A1.type],
    KeyedSubtype.singleEvent[A2.type],
    KeyedSubtype.singleEvent[B1.type],
    KeyedSubtype.singleEvent[B2.type])

  private case class ASnapshot(string: String)

  implicit private val SnapshotJsonCodec = TypedJsonCodec[Any](
    Subtype(deriveCodec[ASnapshot]))
}
