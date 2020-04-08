package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.data.event.{EventId, JournalId}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] trait JournalingObserver
{
  protected[journal] def onJournalingStarted(file: Path, expectedJournalId: JournalId,
    tornLengthAndEventId: PositionAnd[EventId], flushedLengthAndEventId: PositionAnd[EventId]): Unit

  protected[journal] def onJournalingEnded(fileLength: Long): Unit

  protected[journal] def onFileWritten(flushedPosition: Long): Unit

  protected[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit

  protected[journal] def releaseEvents(untilEventId: EventId): Unit

  final def onFileWrittenAndEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    onFileWritten(positionAndEventId.position)
    onEventsCommitted(positionAndEventId, n)
  }
}
