package js7.journal.recover

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.SetOnce
import js7.common.jsonseq.InputStreamJsonSeqReader
import js7.common.utils.UntilNoneIterator
import js7.data.event.{EventId, SnapshotableState}
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.JournalProgress.{AfterSnapshotSection, InCommittedEventsSection}
import js7.journal.recover.StateRecoverer.*
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

private final class StateRecoverer[S <: SnapshotableState[S]](
  protected val file: Path,
  journalLocation: JournalLocation,
  newFileJournaledStateBuilder: () => FileSnapshotableStateBuilder[S])
  (implicit S: SnapshotableState.Companion[S]):

  private val fileJournaledStateBuilder = newFileJournaledStateBuilder()

  private var _position = 0L
  private var _lastProperEventPosition = 0L
  private val _firstEventPosition = SetOnce[Long]

  def recoverAll(): Unit =
    logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
    // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
    autoClosing(InputStreamJsonSeqReader.open(file)) { jsonReader =>
      for json <- UntilNoneIterator(jsonReader.read()).map(_.value) do
        fileJournaledStateBuilder.put(S.decodeJournalJson(json).orThrow)
        fileJournaledStateBuilder.journalProgress match
          case AfterSnapshotSection =>
            _position = jsonReader.position
          case InCommittedEventsSection =>
            _position = jsonReader.position
            _lastProperEventPosition = jsonReader.position
          case _ =>
        if _firstEventPosition.isEmpty && fileJournaledStateBuilder.journalProgress == InCommittedEventsSection then
          _firstEventPosition := jsonReader.position
      for h <- fileJournaledStateBuilder.fileJournalHeader if journalLocation.file(h.eventId) != file do
        sys.error(s"JournalHeaders eventId=${h.eventId} does not match the filename '${file.getFileName}'")
      fileJournaledStateBuilder.logStatistics()
    }

  def firstEventPosition = _firstEventPosition.toOption

  def position: Long = _position

  def lastProperEventPosition: Long = _lastProperEventPosition


object StateRecoverer:
  private val logger = Logger[this.type]

  def resource[S <: SnapshotableState[S]](journalLocation: JournalLocation, config: Config)
    (using S: SnapshotableState.Companion[S], ioRuntime: IORuntime)
  : ResourceIO[Recovered[S]] =
    Resource.fromAutoCloseable(IO(
      StateRecoverer.recover[S](journalLocation, config)))

  def recover[S <: SnapshotableState[S]](
    journalLocation: JournalLocation,
    config: Config,
    runningSince: Deadline = now)
    (using S: SnapshotableState.Companion[S], ioRuntime: IORuntime)
  : Recovered[S] =
    val file = journalLocation.currentFile.toOption
    val fileJournaledStateBuilder = new FileSnapshotableStateBuilder(
      journalFileForInfo = file getOrElse journalLocation.file(EventId.BeforeFirst)/*the expected new filename*/,
      expectedJournalId = None)

    file match
      case Some(file) =>
        val recoverer = new StateRecoverer(file, journalLocation, () => fileJournaledStateBuilder)
        recoverer.recoverAll()
        val nextJournalHeader = fileJournaledStateBuilder.nextJournalHeader
          .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'"))
        Recovered.fromJournalFile(
          journalLocation,
          RecoveredJournalFile(
            file,
            length = recoverer.position,
            lastProperEventPosition = recoverer.lastProperEventPosition,
            journalHeader = fileJournaledStateBuilder.fileJournalHeader
              .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'")),
            nextJournalHeader,
            firstEventPosition = recoverer.firstEventPosition
              .getOrElse(sys.error(s"Missing JournalHeader in file '${file.getFileName}'")),
            fileJournaledStateBuilder.result()),
          totalRunningSince = runningSince - nextJournalHeader.totalRunningTime,
          config)

      case None =>
        // An active cluster node will start a new journal
        // A passive cluster node will provide the JournalId later
        Recovered.noJournalFile(journalLocation, runningSince, config)
