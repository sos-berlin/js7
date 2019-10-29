package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.recover.JournaledStateRecoverer._
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, JournaledState}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import scala.concurrent.duration.{Duration, FiniteDuration}

private final class JournaledStateRecoverer[S <: JournaledState[S, E], E <: Event](
  protected val file: Path,
  expectedJournalId: Option[JournalId],
  journalMeta: JournalMeta,
  protected val stateBuilder: JournalStateBuilder[S, E])
{
  private val journalFileStateBuilder = new JournalFileStateBuilder[S, E](journalMeta, file, expectedJournalId, stateBuilder)
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
  : Recovered[S, E] =
    JournalFiles.currentFile(journalMeta.fileBase) match {
      case Right(file) =>
        val recoverer = new JournaledStateRecoverer(file, expectedJournalId = None, journalMeta, stateBuilder)
        recoverer.recoverAll()
        new Recovered(journalMeta,stateBuilder, config,
          Some(PositionAnd(recoverer.position, file)), stateBuilder.recoveredJournalHeader, Some(stateBuilder.state))

      case Left(_) =>
        new Recovered(journalMeta, stateBuilder, config, None, None, None)
    }
}
