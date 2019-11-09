package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.recover.JournaledStateRecoverer._
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

private final class JournaledStateRecoverer[S <: JournaledState[S, E], E <: Event](
  protected val file: Path,
  expectedJournalId: Option[JournalId],
  journalMeta: JournalMeta,
  newJournalFileStateBuilder: () => JournalFileStateBuilder[S, E])
{
  private val journalFileStateBuilder = newJournalFileStateBuilder()

  private var _position = 0L

  def recoverAll(): Unit = {
    logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
    // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
    autoClosing(InputStreamJsonSeqReader.open(file)) { jsonReader =>
      for (json <- UntilNoneIterator(jsonReader.read()).map(_.value)) {
        journalFileStateBuilder.put(json)
        _position = jsonReader.position
      }
      for (h <- journalFileStateBuilder.fileJournalHeader if journalMeta.file(h.eventId) != file) {
        sys.error(s"JournalHeaders eventId=${h.eventId} does not match the filename '${file.getFileName}'")
      }
      journalFileStateBuilder.logStatistics()
    }
  }

  def position: Long = _position
}

object JournaledStateRecoverer
{
  private val logger = Logger(getClass)

  def recover[S <: JournaledState[S, E], E <: Event](
    journalMeta: JournalMeta,
    newStateBuilder: () => JournalStateBuilder[S, E],
    config: Config)
  : Recovered[S, E] = {
    val file = JournalFiles.currentFile(journalMeta.fileBase).toOption
    val journalFileStateBuilder = new JournalFileStateBuilder[S, E](
      journalMeta,
      journalFileForInfo = file getOrElse journalMeta.file(EventId.BeforeFirst)/*the expected new filename*/,
      expectedJournalId = None,
      newStateBuilder)
    val eventWatch = new JournalEventWatch(journalMeta, config)  // Closed with `Recovered#close`

    file match {
      case Some(file) =>
        val recoverer = new JournaledStateRecoverer(file, expectedJournalId = None, journalMeta, () => journalFileStateBuilder)
        recoverer.recoverAll()
        Recovered(
          journalMeta,
          eventId = journalFileStateBuilder.eventId,
          journalFileStateBuilder.totalRunningTime,
          Some(PositionAnd(recoverer.position, file)),
          journalFileStateBuilder.fileJournalHeader,
          journalFileStateBuilder.recoveredJournalHeader,
          Some(journalFileStateBuilder.state),
          newStateBuilder,
          eventWatch,
          config)

      case None =>
        Recovered(journalMeta, eventId = EventId.BeforeFirst, Duration.Zero, None, None, None, None, newStateBuilder, eventWatch, config)
    }
  }
}
