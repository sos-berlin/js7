package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.recover.JournalProgress.InCommittedEventsSection
import com.sos.jobscheduler.core.event.journal.recover.JournaledStateRecoverer._
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

private final class JournaledStateRecoverer[S <: JournaledState[S, E], E <: Event](
  protected val file: Path,
  expectedJournalId: Option[JournalId],
  journalMeta: JournalMeta,
  newfileJournaledStateBuilder: () => FileJournaledStateBuilder[S, E])
{
  private val fileJournaledStateBuilder = newfileJournaledStateBuilder()

  private var _position = 0L
  private val _firstEventPosition = SetOnce[Long]

  def recoverAll(): Unit = {
    logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
    // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
    autoClosing(InputStreamJsonSeqReader.open(file)) { jsonReader =>
      for (json <- UntilNoneIterator(jsonReader.read()).map(_.value)) {
        fileJournaledStateBuilder.put(json)
        fileJournaledStateBuilder.journalProgress match {
          case JournalProgress.AfterSnapshotSection |
               JournalProgress.InCommittedEventsSection |
               JournalProgress.AfterEventsSection =>
            _position = jsonReader.position
          case _ =>
        }
        if (_firstEventPosition.isEmpty && fileJournaledStateBuilder.journalProgress == InCommittedEventsSection) {
          _firstEventPosition := jsonReader.position
        }
      }
      for (h <- fileJournaledStateBuilder.fileJournalHeader if journalMeta.file(h.eventId) != file) {
        sys.error(s"JournalHeaders eventId=${h.eventId} does not match the filename '${file.getFileName}'")
      }
      fileJournaledStateBuilder.logStatistics()
    }
  }

  def firstEventPosition = _firstEventPosition.toOption

  def position: Long = _position
}

object JournaledStateRecoverer
{
  private val logger = Logger(getClass)

  def recover[S <: JournaledState[S, E], E <: Event](
    journalMeta: JournalMeta,
    newStateBuilder: () => JournaledStateBuilder[S, E],
    config: Config,
    runningSince: Deadline = now)
  : Recovered[S, E] = {
    val file = JournalFiles.currentFile(journalMeta.fileBase).toOption
    val fileJournaledStateBuilder = new FileJournaledStateBuilder[S, E](
      journalMeta,
      journalFileForInfo = file getOrElse journalMeta.file(EventId.BeforeFirst)/*the expected new filename*/,
      expectedJournalId = None,
      newStateBuilder)
    val eventWatch = new JournalEventWatch(journalMeta, config)  // Closed with `Recovered#close`

    file match {
      case Some(file) =>
        val recoverer = new JournaledStateRecoverer(file, expectedJournalId = None, journalMeta, () => fileJournaledStateBuilder)
        recoverer.recoverAll()
        val calculatedJournalHeader = fileJournaledStateBuilder.calculatedJournalHeader
          .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'"))
        Recovered(
          journalMeta,
          Some(RecoveredJournalFile(
            file,
            length = recoverer.position,
            fileJournaledStateBuilder.fileJournalHeader getOrElse sys.error(s"Missing JournalHeader in file '${file.getFileName}'"),
            calculatedJournalHeader,
            firstEventPosition = recoverer.firstEventPosition getOrElse sys.error(s"Missing JournalHeader in file '${file.getFileName}'"),
            fileJournaledStateBuilder.state)),
          totalRunningSince = runningSince - calculatedJournalHeader.totalRunningTime,
          newStateBuilder,
          eventWatch,
          config)

      case None =>
        Recovered(journalMeta, None, runningSince, newStateBuilder, eventWatch, config)
    }
  }
}
