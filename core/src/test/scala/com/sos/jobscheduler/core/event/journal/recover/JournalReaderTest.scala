package com.sos.jobscheduler.core.event.journal.recover

import akka.pattern.ask
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.test.{TestActor, TestAggregate, TestAggregateActor, TestEvent, TestJournalMixin}
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.event.{EventId, Stamped}
import java.nio.file.Files.delete
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalReaderTest extends FreeSpec with TestJournalMixin {

  "Journal file without snapshots or events" in {
    withTestActor { (_, _ ) ⇒ }
    val file = currentFile
    autoClosing(new JournalReader(journalMeta, file)) { journalReader ⇒
      assert(journalReader.nextSnapshots().toList == Nil)
      assert(journalReader.nextEvents().toList == Nil)
      assert(journalReader.eventId == EventId.BeforeFirst)
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
      //assert(journalReader.tornEventId == 0)
      assert(journalReader.eventId == EventId.BeforeFirst)
      assert(journalReader.nextSnapshots().toList == Nil)
      assert(journalReader.nextEvents().toList == Stamped(1001, "X" <-: TestEvent.Removed) :: Nil)
      assert(journalReader.eventId == 1001)
    }
  }

  "Journal file with snapshot and events" in {
    withTestActor { (actorSystem, actor) ⇒
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
    }
  }

  private def currentFile = JournalFiles.currentFile(journalMeta.fileBase).orThrow
}
