package js7.core.event.journal.watch

import java.nio.file.Path
import js7.common.event.PositionAnd
import js7.data.event.{EventId, JournalId}
import monix.execution.Scheduler

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

  protected[journal] def releaseEvents(untilEventId: EventId)(implicit s: Scheduler): Unit

  final def onFileWrittenAndEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    onFileWritten(positionAndEventId.position)
    onEventsCommitted(positionAndEventId, n)
  }
}
