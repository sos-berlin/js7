package js7.core.event.journal.recover

import akka.pattern.ask
import akka.util.ByteString
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.common.scalautil.Futures.implicits._
import js7.core.event.journal.JournalActor
import js7.core.event.journal.files.JournalFiles
import js7.core.event.journal.test.{TestActor, TestAggregate, TestAggregateActor, TestEvent, TestJournalMixin}
import js7.core.event.journal.write.{EventJournalWriter, FileJsonWriter, SnapshotJournalWriter}
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalEvent, JournalHeader, JournalId, JournalSeparators, KeyedEvent, Stamped}
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import java.nio.file.Files.delete
import java.util.UUID
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalReaderTest extends AnyFreeSpec with TestJournalMixin
{
  private val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))

  "Journal file without snapshots or events" in {
    withTestActor() { (_, _ ) => }
    val file = currentFile
    autoClosing(new JournalReader(journalMeta, Some(journalId), file)) { journalReader =>
      assert(journalReader.nextSnapshots().toList == journalReader.journalHeader :: Nil)
      assert(journalReader.nextEvents().toList == Stamped(1000000L, (NoKey <-: JournalEvent.SnapshotTaken)) :: Nil)
      assert(journalReader.eventId == 1000000)
      assert(journalReader.totalEventCount == 1)
    }
  }

  "Journal file with snapshot section only" in {
    val file = currentFile
    delete(file)  // File of last test
    autoClosing(new SnapshotJournalWriter(journalMeta, file, after = EventId.BeforeFirst, simulateSync = None)) { writer =>
      writer.writeHeader(JournalHeader.forTest(journalId))
      writer.beginSnapshotSection()
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(1000L, NoKey <-: SnapshotTaken))
    }
    autoClosing(new JournalReader(journalMeta, Some(journalId), JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader =>
      assert(journalReader.nextSnapshots().toList == journalReader.journalHeader :: Nil)
      assert(journalReader.nextEvents().toList == Stamped(1000L, (NoKey <-: JournalEvent.SnapshotTaken)) :: Nil)
      assert(journalReader.eventId == 1000)
      assert(journalReader.totalEventCount == 1)
    }
  }

  "Journal file with open event section" in {
    val file = currentFile
    delete(file)  // File of last test
    autoClosing(new SnapshotJournalWriter(journalMeta, file, after = EventId.BeforeFirst, simulateSync = None)) { writer =>
      writer.writeHeader(JournalHeader.forTest(journalId))
      writer.beginSnapshotSection()
      writer.endSnapshotSection()
      writer.beginEventSection(sync = false)
      writer.writeEvent(Stamped(1000L, NoKey <-: SnapshotTaken))
    }
    autoClosing(new EventJournalWriter(journalMeta, file, after = 1000L, journalId, observer = None, simulateSync = None)) { writer =>
      writer.writeEvents(Stamped(1001L, "X" <-: TestEvent.Removed) :: Nil)
      //Without: writer.endEventSection(sync = false)
    }
    autoClosing(new JournalReader(journalMeta, Some(journalId), JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader =>
      assert(journalReader.tornEventId == 0)
      assert(journalReader.eventId == EventId.BeforeFirst)
      assert(journalReader.nextSnapshots().toList == journalReader.journalHeader :: Nil)
      assert(journalReader.nextEvents().toList == Stamped(1000L, NoKey <-: SnapshotTaken) :: Stamped(1001L, "X" <-: TestEvent.Removed) :: Nil)
      assert(journalReader.eventId == 1001)
      assert(journalReader.totalEventCount == 2)
    }
  }

  "Journal file with snapshot and events" in {
    withTestActor() { (actorSystem, actor) =>
      for ((key, cmd) <- testCommands("TEST")) execute(actorSystem, actor, key, cmd) await 99.s
      (actor ? TestActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] await 99.s
      execute(actorSystem, actor, "X", TestAggregateActor.Command.Add("(X)")) await 99.s
      execute(actorSystem, actor, "Y", TestAggregateActor.Command.Add("(Y)")) await 99.s
    }
    autoClosing(new JournalReader(journalMeta, Some(journalId), currentFile)) { journalReader =>
      assert(journalReader.nextSnapshots().toSet == Set(
        journalReader.journalHeader,
        TestAggregate("TEST-A","(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C","(C.Add)")))
      assert(journalReader.nextEvents().toList == List(
        Stamped(1000067L, NoKey <-: SnapshotTaken),
        Stamped(1000068L, "X" <-: TestEvent.Added("(X)")),
        Stamped(1000069L, "Y" <-: TestEvent.Added("(Y)"))))
      assert(journalReader.eventId == 1000069)
      assert(journalReader.totalEventCount == 72)
    }
  }

  "Transactions" - {
    val first = Stamped(1000L, "A" <-: TestEvent.Removed)
    val ta = Stamped(1001L, "KEY" <-: TestEvent.Appended('a')) ::
             Stamped(1002L, "KEY" <-: TestEvent.Appended('b')) ::
             Stamped(1003L, "KEY" <-: TestEvent.Appended('c')) :: Nil
    val last = Stamped(1004L, "Z" <-: TestEvent.Removed)

    "Committed transaction" in {
      val file = currentFile
      delete(file)  // File of last test
      autoClosing(new EventJournalWriter(journalMeta, file, after = 0L, journalId, observer = None, simulateSync = None, withoutSnapshots = true)) { writer =>
        writer.writeHeader(JournalHeader.forTest(journalId, eventId = EventId.BeforeFirst))
        writer.beginEventSection(sync = false)
        writer.writeEvents(first :: Nil)
        writer.writeEvents(ta, transaction = true)
        writer.writeEvents(last :: Nil)
        writer.endEventSection(sync = false)
      }
      autoClosing(new JournalReader(journalMeta, Some(journalId), JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader =>
        assert(journalReader.nextEvents().toList == first :: ta ::: last :: Nil)
        assert(journalReader.eventId == 1004)
        assert(journalReader.totalEventCount == 5)
      }
      autoClosing(new JournalReader(journalMeta, Some(journalId), JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader =>
        assert(journalReader.nextEvent() == Some(first))
        assert(journalReader.eventId == 1000)
        assert(journalReader.nextEvent() == Some(ta(0)))
        assert(journalReader.eventId == 1001)
        assert(journalReader.nextEvent() == Some(ta(1)))
        assert(journalReader.eventId == 1002)
        assert(journalReader.nextEvent() == Some(ta(2)))
        assert(journalReader.eventId == 1003)
        assert(journalReader.nextEvent() == Some(last))
        assert(journalReader.eventId == 1004)
        assert(journalReader.nextEvent() == None)
        assert(journalReader.eventId == 1004)
        assert(journalReader.totalEventCount == 5)
      }
    }

    "Uncommitted transaction" in {
      val file = currentFile
      delete(file)  // File of last test
      autoClosing(new FileJsonWriter(file)) { writer =>
        def write[A](a: A)(implicit encoder: Encoder[A]) = writer.write(ByteString(encoder(a).compactPrint))
        def writeEvent(a: Stamped[KeyedEvent[TestEvent]]) = write(a)

        write(JournalHeader.forTest(journalId, eventId = EventId.BeforeFirst).asJson)
        write(JournalSeparators.EventHeader)
        writeEvent(first)
        write(JournalSeparators.Transaction)
        writeEvent(ta(0))
        writeEvent(ta(1))
        writeEvent(ta(2))
      }
      autoClosing(new JournalReader(journalMeta, Some(journalId), JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader =>
        assert(journalReader.nextEvents().toList == first :: Nil)  // Uncommitted transaction is not read
        assert(journalReader.eventId == 1000)
        assert(journalReader.totalEventCount == 1)
      }
    }
  }

  private def currentFile = JournalFiles.currentFile(journalMeta.fileBase).orThrow
}
