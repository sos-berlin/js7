package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.data.event.{EventId, JournalId}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] trait JournalingObserver
{
  protected[journal] def onJournalingStarted(file: Path, flushedLengthAndEventId: PositionAnd[EventId], expectedJournalId: JournalId): Unit

  protected[journal] def onJournalingEnded(fileLength: Long): Unit

  protected[journal] def onFileWritten(flushedPosition: Long): Unit

  protected[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit

  protected[journal] def keepEvents(eventId: EventId): Checked[Completed]

  protected[journal] def deleteObsoleteJournalFiles(): Unit
}
