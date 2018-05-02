package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] object TestRecoverer
{
  def recover(journalMeta: JournalMeta[TestEvent], journalFile: Path): TestState =
    autoClosing(new JournalReader(journalMeta, journalFile)) { journalReader ⇒
      var state = TestState(Map.empty)
      UntilNoneIterator { journalReader.recoverNext() } foreach {
        case JournalReader.RecoveredSnapshot(aggregate: TestAggregate) ⇒
          state = state.applySnapshot(aggregate)

        case JournalReader.RecoveredEvent(Stamped(_, _, KeyedEvent(key: String, event: TestEvent))) ⇒
          state = state.applyEvent(key <-: event)
      }
      state
    }
}
