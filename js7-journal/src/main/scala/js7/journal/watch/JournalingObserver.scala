package js7.journal.watch

import cats.effect.unsafe.IORuntime
import com.typesafe.scalalogging.Logger
import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalId}
import js7.journal.FileJournal.logger
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.listJournalFiles
import scala.util.control.NonFatal

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

  private val logger = Logger[this.type]
  
  final class Dummy(journalLocation: JournalLocation) extends JournalingObserver:
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
      // Without a JournalingObserver, we can delete all previous journal files (for Agent)
      val until = untilEventId
      for j <- journalLocation.listJournalFiles if j.fileEventId < until do
        val file = j.file
        //assertThat(file != eventWriter.file)
        try delete(file)
        catch case NonFatal(t) =>
          logger.warn(s"Cannot delete obsolete journal file '$file': ${t.toStringWithCauses}")
