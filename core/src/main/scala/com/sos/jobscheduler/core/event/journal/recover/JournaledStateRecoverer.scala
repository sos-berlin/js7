package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.recover.JournaledStateRecoverer._
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}

private final class JournaledStateRecoverer[S <: JournaledState[S, E], E <: Event](
  protected val file: Path,
  expectedJournalId: Option[JournalId],
  journalMeta: JournalMeta,
  journalFileStateBuilder: JournalFileStateBuilder[S, E])
{
  protected def stateBuilder = journalFileStateBuilder.builder

  private var _journalHeader = SetOnce[JournalHeader]
  private var _position = 0L

  def recoverAll(): Unit = {
    logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
    // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
    autoClosing(InputStreamJsonSeqReader.open(file)) { jsonReader =>
      for (json <- UntilNoneIterator(jsonReader.read()).map(_.value)) {
        journalFileStateBuilder.put(json)
        _position = jsonReader.position
      }
      _journalHeader := stateBuilder.journalHeader
      stateBuilder.logStatistics()
    }
  }

  def journalHeader: JournalHeader =
    _journalHeader()

  def position: Long = _position
}

object JournaledStateRecoverer
{
  private val logger = Logger(getClass)

  def recover[S <: JournaledState[S, E], E <: Event](
    journalMeta: JournalMeta,
    stateBuilder: JournalStateBuilder[S, E],
    config: Config)
  : Recovered[S, E] = {
    val file = JournalFiles.currentFile(journalMeta.fileBase).toOption
    val journalFileStateBuilder = new JournalFileStateBuilder[S, E](
      journalMeta,
      journalFileForInfo = file getOrElse journalMeta.file(EventId.BeforeFirst)/*the expected new filename*/,
      expectedJournalId = None,
      stateBuilder)

    file match {
      case Some(file) =>
        val recoverer = new JournaledStateRecoverer(file, expectedJournalId = None, journalMeta, journalFileStateBuilder)
        recoverer.recoverAll()
        new Recovered(journalMeta, journalFileStateBuilder, config,
          Some(PositionAnd(recoverer.position, file)), journalFileStateBuilder.recoveredJournalHeader, Some(journalFileStateBuilder.state))

      case None =>
        new Recovered(journalMeta, journalFileStateBuilder, config, None, None, None)
    }
  }
}
