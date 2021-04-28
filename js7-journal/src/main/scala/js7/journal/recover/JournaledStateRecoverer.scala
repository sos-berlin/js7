package js7.journal.recover

import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.SetOnce
import js7.common.jsonseq.InputStreamJsonSeqReader
import js7.common.utils.UntilNoneIterator
import js7.data.event.{EventId, JournaledState}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.JournalProgress.{AfterSnapshotSection, InCommittedEventsSection}
import js7.journal.recover.JournaledStateRecoverer._
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

private final class JournaledStateRecoverer[S <: JournaledState[S]](
  protected val file: Path,
  journalMeta: JournalMeta,
  newfileJournaledStateBuilder: () => FileJournaledStateBuilder[S])
{
  private val fileJournaledStateBuilder = newfileJournaledStateBuilder()

  private var _position = 0L
  private var _lastProperEventPosition = 0L
  private val _firstEventPosition = SetOnce[Long]

  def recoverAll(): Unit = {
    logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
    // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
    autoClosing(InputStreamJsonSeqReader.open(file)) { jsonReader =>
      for (json <- UntilNoneIterator(jsonReader.read()).map(_.value)) {
        fileJournaledStateBuilder.put(journalMeta.decodeJson(json).orThrow)
        fileJournaledStateBuilder.journalProgress match {
          case AfterSnapshotSection =>
            _position = jsonReader.position
          case InCommittedEventsSection =>
            _position = jsonReader.position
            _lastProperEventPosition = jsonReader.position
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

  def lastProperEventPosition: Long = _lastProperEventPosition
}

object JournaledStateRecoverer
{
  private val logger = Logger(getClass)

  def recover[S <: JournaledState[S]](
    journalMeta: JournalMeta,
    config: Config,
    runningSince: Deadline = now)
    (implicit S: JournaledState.Companion[S])
  : Recovered[S] = {
    val file = JournalFiles.currentFile(journalMeta.fileBase).toOption
    val fileJournaledStateBuilder = new FileJournaledStateBuilder(
      journalFileForInfo = file getOrElse journalMeta.file(EventId.BeforeFirst)/*the expected new filename*/,
      expectedJournalId = None)

    file match {
      case Some(file) =>
        val recoverer = new JournaledStateRecoverer(file, journalMeta, () => fileJournaledStateBuilder)
        recoverer.recoverAll()
        val nextJournalHeader = fileJournaledStateBuilder.nextJournalHeader
          .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'"))
        Recovered(
          journalMeta,
          Some(RecoveredJournalFile(
            file,
            length = recoverer.position,
            lastProperEventPosition = recoverer.lastProperEventPosition,
            journalHeader = fileJournaledStateBuilder.fileJournalHeader
              .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'")),
            nextJournalHeader,
            firstEventPosition = recoverer.firstEventPosition
              .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'")),
            fileJournaledStateBuilder.result())),
          totalRunningSince = runningSince - nextJournalHeader.totalRunningTime,
          config)

      case None =>
        // An active cluster node will start a new journal
        // A passive cluster node will provide the JournalId later
        Recovered[S](journalMeta, None, runningSince, config)
    }
  }
}
