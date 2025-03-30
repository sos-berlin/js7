package js7.journal.watch

import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalId}

/**
  * @author Joacim Zschimmer
  */
private[journal] trait JournalingObserver:

  protected[journal] def onJournalingStarted(
    file: Path,
    expectedJournalId: JournalId,
    firstEventPositionAndFileEventId: PositionAnd[EventId],
    flushedLengthAndEventId: PositionAnd[EventId],
    isActiveNode: Boolean): Unit

  protected[journal] def onJournalingEnded(fileLength: Long): Unit

  protected[journal] def onFileWritten(flushedPosition: Long): Unit

  protected[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit

  protected[journal] def releaseEvents(untilEventId: EventId)(using IORuntime): Unit

  final def onFileWrittenAndEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int)
  : Unit =
    onFileWritten(positionAndEventId.position)
    onEventsCommitted(positionAndEventId, n)


private[journal] object JournalingObserver:

  object Dummy extends JournalingObserver:
    override def toString = "JournalingObserver.Dummy"

    protected[journal] def onJournalingStarted(
      file: Path,
      expectedJournalId: JournalId,
      firstEventPositionAndFileEventId: PositionAnd[EventId],
      flushedLengthAndEventId: PositionAnd[EventId],
      isActiveNode: Boolean)
    : Unit =
      ()

    protected[journal] def onJournalingEnded(fileLength: EventId): Unit =
      ()

    protected[journal] def onFileWritten(flushedPosition: EventId): Unit =
      ()

    protected[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int)
    : Unit =
      ()

    protected[journal] def releaseEvents(untilEventId: EventId)(using IORuntime): Unit =
      ()
