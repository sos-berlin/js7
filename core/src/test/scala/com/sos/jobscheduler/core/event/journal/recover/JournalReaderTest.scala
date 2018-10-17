package com.sos.jobscheduler.core.event.journal.recover

import akka.pattern.ask
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalHeaders}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.test.{TestActor, TestAggregate, TestAggregateActor, TestEvent, TestJournalMixin}
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, FileJsonWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped}
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import java.nio.file.Files.delete
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalReaderTest extends FreeSpec with TestJournalMixin {

  "Journal file without snapshots or events" in {
    withTestActor() { (_, _ ) ⇒ }
    val file = currentFile
    autoClosing(new JournalReader(journalMeta, file)) { journalReader ⇒
      assert(journalReader.nextSnapshots().toList == Nil)
      assert(journalReader.nextEvents().toList == Nil)
      assert(journalReader.eventId == EventId.BeforeFirst)
      assert(journalReader.totalEventCount == 0)
    }
  }

  "Journal file with snapshot section only" in {
    val file = currentFile
    delete(file)  // File of last test
    autoClosing(new SnapshotJournalWriter(journalMeta, file, observer = None, simulateSync = None)) { writer ⇒
      writer.writeHeader(JournalHeader(eventId = EventId.BeforeFirst, totalEventCount = 0))
      writer.beginSnapshotSection()
      writer.endSnapshotSection(sync = false)
    }
    autoClosing(new JournalReader(journalMeta, JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader ⇒
      assert(journalReader.nextSnapshots().toList == Nil)
      assert(journalReader.nextEvents().toList == Nil)
      assert(journalReader.eventId == EventId.BeforeFirst)
      assert(journalReader.totalEventCount == 0)
    }
  }

  "Journal file with open event section" in {
    val file = currentFile
    delete(file)  // File of last test
    autoClosing(new SnapshotJournalWriter(journalMeta, file, observer = None, simulateSync = None)) { writer ⇒
      writer.writeHeader(JournalHeader(eventId = EventId.BeforeFirst, totalEventCount = 0))
      writer.beginSnapshotSection()
      writer.endSnapshotSection(sync = false)
    }
    autoClosing(new EventJournalWriter(journalMeta, file, observer = None, simulateSync = None, after = 0)) { writer ⇒
      writer.beginEventSection()
      writer.writeEvents(Stamped(1001, "X" <-: TestEvent.Removed) :: Nil)
      //Without: writer.endEventSection(sync = false)
    }
    autoClosing(new JournalReader(journalMeta, JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader ⇒
      assert(journalReader.tornEventId == 0)
      assert(journalReader.eventId == EventId.BeforeFirst)
      assert(journalReader.nextSnapshots().toList == Nil)
      assert(journalReader.nextEvents().toList == Stamped(1001, "X" <-: TestEvent.Removed) :: Nil)
      assert(journalReader.eventId == 1001)
      assert(journalReader.totalEventCount == 1)
    }
  }

  "Journal file with snapshot and events" in {
    withTestActor() { (actorSystem, actor) ⇒
      for ((key, cmd) ← testCommands("TEST")) execute(actorSystem, actor, key, cmd) await 99.s
      (actor ? TestActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] await 99.s
      execute(actorSystem, actor, "X", TestAggregateActor.Command.Add("(X)")) await 99.s
      execute(actorSystem, actor, "Y", TestAggregateActor.Command.Add("(Y)")) await 99.s
    }
    autoClosing(new JournalReader(journalMeta, currentFile)) { journalReader ⇒
      assert(journalReader.nextSnapshots().toSet == Set(
        TestAggregate("TEST-A","(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"),
        TestAggregate("TEST-C","(C.Add)")))
      assert(journalReader.nextEvents().toList == List(
        Stamped(1000066, "X" <-: TestEvent.Added("(X)")),
        Stamped(1000067, "Y" <-: TestEvent.Added("(Y)"))))
      assert(journalReader.eventId == 1000067)
      assert(journalReader.totalEventCount == 69)
    }
  }

  "Transactions" - {
    val first = Stamped(1000, "A" <-: TestEvent.Removed)
    val ta = Stamped(1001, "KEY" <-: TestEvent.Appended('a')) ::
             Stamped(1002, "KEY" <-: TestEvent.Appended('b')) ::
             Stamped(1003, "KEY" <-: TestEvent.Appended('c')) :: Nil
    val last = Stamped(1004, "Z" <-: TestEvent.Removed)

    "Committed transaction" in {
      val file = currentFile
      delete(file)  // File of last test
      autoClosing(new EventJournalWriter(journalMeta, file, observer = None, simulateSync = None, after = 0, withoutSnapshots = true)) { writer ⇒
        writer.writeHeader(JournalHeader(eventId = EventId.BeforeFirst, totalEventCount = 0))
        writer.beginEventSection()
        writer.writeEvents(first :: Nil)
        writer.writeEvents(ta, transaction = true)
        writer.writeEvents(last :: Nil)
        writer.endEventSection(sync = false)
      }
      autoClosing(new JournalReader(journalMeta, JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader ⇒
        assert(journalReader.nextEvents().toList == first :: ta ::: last :: Nil)
        assert(journalReader.eventId == 1004)
        assert(journalReader.totalEventCount == 5)
      }
    }

    "Uncommitted transaction" in {
      val file = currentFile
      delete(file)  // File of last test
      autoClosing(new FileJsonWriter(file)) { writer ⇒
        import journalMeta.eventJsonCodec
        def write[A](a: A)(implicit encoder: Encoder[A]) = writer.write(ByteString(encoder(a).compactPrint))
        def writeEvent(a: Stamped[KeyedEvent[TestEvent]]) = write(a)

        write(JournalHeader(eventId = EventId.BeforeFirst, totalEventCount = 0).asJson)
        write(JournalHeaders.EventHeader)
        writeEvent(first)
        write(JournalHeaders.Transaction)
        writeEvent(ta(0))
        writeEvent(ta(1))
        writeEvent(ta(2))
      }
      autoClosing(new JournalReader(journalMeta, JournalFiles.currentFile(journalMeta.fileBase).orThrow)) { journalReader ⇒
        assert(journalReader.nextEvents().toList == first :: Nil)  // Uncommitted transaction is not read
        assert(journalReader.eventId == 1000)
        assert(journalReader.totalEventCount == 1)
      }
    }
  }

  private def currentFile = JournalFiles.currentFile(journalMeta.fileBase).orThrow
}
