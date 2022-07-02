package js7.journal.watch

import java.nio.file.Path
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalId}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
private[journal] trait JournalingObserver
{
  protected[journal] def onJournalingStarted(
    file: Path,
    expectedJournalId: JournalId,
    firstEventPositionAndFileEventId: PositionAnd[EventId],
    flushedLengthAndEventId: PositionAnd[EventId],
    isActiveNode: Boolean): Unit

  protected[journal] def onJournalingEnded(fileLength: Long): Unit

  protected[journal] def onFileWritten(flushedPosition: Long): Unit

  protected[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit

  protected[journal] def releaseEvents(untilEventId: EventId)(implicit s: Scheduler): Unit

  final def onFileWrittenAndEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int)
  : Unit = {
    onFileWritten(positionAndEventId.position)
    onEventsCommitted(positionAndEventId, n)
  }
}
