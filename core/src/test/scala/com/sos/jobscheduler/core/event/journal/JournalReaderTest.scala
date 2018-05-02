package com.sos.jobscheduler.core.event.journal

import akka.pattern.ask
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.JournalReader.{RecoveredEvent, RecoveredSnapshot}
import com.sos.jobscheduler.core.event.journal.TestActor.TestJournalMeta
import com.sos.jobscheduler.data.event.Stamped
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalReaderTest extends FreeSpec with TestJournalMixin {

  protected val journalMeta = TestJournalMeta

  "Test" in {
    withTestActor { (actorSystem, actor) ⇒
      for ((key, cmd) ← testCommands("TEST")) execute(actorSystem, actor, key, cmd) await 99.s
      (actor ? TestActor.Input.TakeSnapshot).mapTo[JournalActor.Output.SnapshotTaken.type] await 99.s
      execute(actorSystem, actor, "X", TestAggregateActor.Command.Add("(X)")) await 99.s
      execute(actorSystem, actor, "Y", TestAggregateActor.Command.Add("(Y)")) await 99.s
    }
    autoClosing(new JournalReader(journalMeta, journalFile)) { journalReader ⇒
      assert(Vector.fill(2) { journalReader.recoverNext() } .toSet == Set(
        Some(RecoveredSnapshot(TestAggregate("TEST-A","(A.Add)(A.Append)(A.AppendAsync)(A.AppendNested)(A.AppendNestedAsync)"))),
        Some(RecoveredSnapshot(TestAggregate("TEST-C","(C.Add)")))))
      assert(journalReader.recoverNext() == Some(RecoveredEvent(Stamped(1000066, "X" <-: TestEvent.Added("(X)")))))
      assert(journalReader.recoverNext() == Some(RecoveredEvent(Stamped(1000067, "Y" <-: TestEvent.Added("(Y)")))))
      assert(journalReader.recoverNext() == None)
    }
  }
}
