package js7.journal.watch

import cats.syntax.semigroup.*
import com.typesafe.config.Config
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{delete, exists, size}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.configutils.Configs.*
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.{CheckedOption, Ops}
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{CloseableIterator, SetOnce}
import js7.common.jsonseq.PositionAnd
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournalInfo, JournalPosition, KeyedEvent, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles
import js7.journal.files.JournalFiles.listGarbageFiles
import js7.journal.watch.JournalEventWatch.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** Watches a complete journal consisting of n `JournalFile`.
  * The last one (with highest after-EventId) is the currently written file while the others are historic.
  */
final class JournalEventWatch(
  val journalMeta: JournalMeta,
  config: Config,
  announceNextFileEventId: Option[EventId] = None)
extends AutoCloseable
with RealEventWatch
with FileEventWatch
with JournalingObserver
{
  logger.debug(
    s"new JournalEventWatch($journalMeta, announceNextFileEventId=$announceNextFileEventId)")

  private val keepOpenCount = config.getInt("js7.journal.watch.keep-open")
  private val releaseEventsDelay =
    config.getDuration("js7.journal.release-events-delay").toFiniteDuration max 0.s

  // Read journal file names from directory while constructing
  @volatile private var fileEventIdToHistoric: SortedMap[EventId, HistoricJournalFile] =
    SortedMap.empty[EventId, HistoricJournalFile] ++
      JournalFiles.listJournalFiles(journalMeta.fileBase)
        .map(o => new HistoricJournalFile(o.fileEventId, o.file))
        .toKeyedMap(_.fileEventId)
  for (historic <- fileEventIdToHistoric.values) logger.debug(historic.toString)

  private val journalIdOnce = SetOnce.fromOption[JournalId](
    fileEventIdToHistoric
      .lastOption
      .map(_._2.file.toFile)
      .map(file => Checked
        .catchNonFatal(
          autoClosing(scala.io.Source.fromFile(file)(UTF_8))(_.getLines().next()))
        .flatMap(_.parseJsonAs[JournalHeader])
        .map(_.journalId)
        .mapProblem(Problem.pure(s"Invalid journal file '$file': ") |+| _)
        .orThrow))

  private val startedPromise = Promise[this.type]()
  @volatile
  private var maybeCurrentEventReader: Option[CurrentEventReader] = None

  // announceNextFileEventId, the recovered EventId,
  // optionally announces the next journal file
  // to avoid "Unknown journal file" if PassiveClusterNode starts replication of
  // the next journal file before this active node call onJournalingStarted.
  // This may happen especially when the node starts with a big snapshot
  // which delays onJournalingStarted.
  @volatile
  private var announcedEventReaderPromise: Option[(EventId, Promise[Option[CurrentEventReader]])] =
    announceNextFileEventId.map(_ -> Promise())

  @volatile
  private var _isActiveNode = false

  def close() = {
    fileEventIdToHistoric.values.foreach(_.close())
    maybeCurrentEventReader.foreach(_.close())
    for (o <- announcedEventReaderPromise)
      o._2.trySuccess(None)
  }

  override def whenStarted = startedPromise.future

  def isActiveNode = _isActiveNode

  def onJournalingStarted(
    file: Path,
    expectedJournalId: JournalId,
    firstEventPositionAndFileEventId: PositionAnd[EventId],
    flushedLengthAndEventId: PositionAnd[EventId],
    isActiveNode: Boolean)
  : Unit = {
    // firstEventPositionAndFileEventId and flushedLengthAndEventId may differ only when
    // PassiveClusterNode continues replicating an existing journal file.
    logger.debug(s"onJournalingStarted ${file.getFileName}, " +
      s"firstEventPositionAndFileEventId=$firstEventPositionAndFileEventId, " +
      s"flushedLengthAndEventId=$flushedLengthAndEventId")
    journalIdOnce.toOption match {
      case None => journalIdOnce := expectedJournalId
      case Some(o) => require(expectedJournalId == o, s"JournalId $o does not match expected $expectedJournalId")
        //throw JournalIdMismatchProblem(journalMeta.fileBase, expectedJournalId = expectedJournalId, o).throwable
    }
    synchronized {
      _isActiveNode = isActiveNode
      val after = flushedLengthAndEventId.value
      if (after < lastEventId) throw new IllegalArgumentException(
        s"Invalid onJournalingStarted(after=$after), must be ≥ $lastEventId")
      for (current <- maybeCurrentEventReader) {
        if (file == current.journalFile) sys.error(
          s"onJournalingStarted: file == current.journalFile == ${file.getFileName}")
        if (current.lastEventId != firstEventPositionAndFileEventId.value)
          throw new IllegalArgumentException(
            s"onJournalingStarted(${firstEventPositionAndFileEventId.value}) " +
              s"does not match lastEventId=${current.lastEventId}")
        for (o <- fileEventIdToHistoric.get(current.fileEventId)) {
          // In case last journal file had no events (and `after` remains), we exchange it
          o.closeAfterUse()
        }
        val historic = new HistoricJournalFile(
          fileEventId = current.fileEventId,
          current.journalFile,
          Some(current)/*Reuse built-up JournalIndex*/)
        logger.debug(s"Add $historic")
        fileEventIdToHistoric += current.fileEventId -> historic
      }

      val currentEventReader = new CurrentEventReader(journalMeta, expectedJournalId,
        firstEventPositionAndFileEventId, flushedLengthAndEventId,
        isActiveNode = isActiveNode, config)
      maybeCurrentEventReader = Some(currentEventReader)
      logger.debug(s"currentEventReader=$currentEventReader")
      for ((eventId, promise) <- announcedEventReaderPromise) {
        if (eventId == firstEventPositionAndFileEventId.value)
          promise.success(Some(currentEventReader))
      }
    }
    onFileWritten(flushedLengthAndEventId.position)
    onEventsCommitted(flushedLengthAndEventId.value)  // Notify about already written events
    startedPromise.trySuccess(this)
    evictUnusedEventReaders()
  }

  def onJournalingEnded(fileLength: Long) =
    // TODO Delay until no FailedOver event may be written?
    //  This would be after the next journal file has been written with an acknowledged event
    //  - SnapshotTaken is not being acknowledged!
    //  Können wir auf onJournalingEnded verzichten zu Gunsten von onJournalingStarted ?
    for (o <- maybeCurrentEventReader) {
      logger.debug(s"onJournalingEnded ${o.journalFile.getFileName} fileLength=$fileLength")
      announcedEventReaderPromise = Some(o.lastEventId -> Promise())
      o.onJournalingEnded(fileLength)
    }

  def releaseEvents(untilEventId: EventId)(implicit scheduler: Scheduler): Unit = {
    val delay = (EventId.toTimestamp(untilEventId) + releaseEventsDelay) - Timestamp.now
    if (delay.isZeroOrBelow) {
      releaseEventsNow(untilEventId)
    } else {
      logger.debug(s"releaseEvents($untilEventId), delay ${delay.pretty}")
      scheduler.scheduleOnce(delay) {
        releaseEventsNow(untilEventId)
      }
    }
  }

  private def releaseEventsNow(untilEventId: EventId): Unit = {
    logger.debug(s"releaseEventsNow($untilEventId)")
    // Delete only journal files before the file containing `untilFileEventId`
    val untilFileEventId = eventIdToFileEventId(untilEventId)
    deleteJournalFiles(untilFileEventId)
    deleteGarbageFiles(untilFileEventId)
  }

  private def eventIdToFileEventId(eventId: EventId): EventId =
    maybeCurrentEventReader match {
      case Some(current) if current.fileEventId <= eventId =>
        current.fileEventId
      case _ =>
        historicJournalFileAfter(eventId).fold(EventId.BeforeFirst)(_.fileEventId)
    }

  private def deleteJournalFiles(untilFileEventId: EventId): Unit = {
    val obsoletes = fileEventIdToHistoric.values.filter(_.fileEventId < untilFileEventId)
    for (historic <- obsoletes) {
      val filename = historic.file.getFileName
      logger.info(s"Delete obsolete journal file '$filename' " +
        s"(${Try(toKBGB(size(historic.file))).fold(identity, identity)})")
      historic.close()
      try {
        delete(historic.file)
        synchronized {
          logger.debug(s"Remove $historic")
          fileEventIdToHistoric -= historic.fileEventId
        }
      } catch {
        case e: IOException =>
          if (!exists(historic.file))
            synchronized {
              fileEventIdToHistoric -= historic.fileEventId
            }
          else
            logger.warn(
              s"Cannot delete obsolete journal file '$filename': ${e.toStringWithCauses}")
      }
    }
  }

  private def deleteGarbageFiles(untilFileEventId: EventId): Unit =
    for (file <- listGarbageFiles(journalMeta.fileBase, untilFileEventId = untilFileEventId)) {
      logger.info(s"Delete garbage journal file '${file.getFileName}'")
      try delete(file)
      catch { case e: IOException =>
        if (exists(file)) {
          logger.warn(s"Cannot delete garbage journal file '$file': ${e.toStringWithCauses}")
        }
      }
    }

  def onFileWritten(flushedPosition: Long): Unit =
    for (o <- maybeCurrentEventReader) {
      o.onFileWritten(flushedPosition)
    }

  def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    checkedCurrentEventReader.orThrow.onEventsCommitted(positionAndEventId, n = n)
    onEventsCommitted(positionAndEventId.value)
  }

  def snapshotAfter(after: EventId) =
    rawSnapshotAfter(after)
      .map(_.mapParallelBatch()(_.parseJsonAs(journalMeta.snapshotObjectJsonCodec).orThrow))

  def rawSnapshotAfter(after: EventId) =
    maybeCurrentEventReader match {
      case Some(current) if current.fileEventId <= after =>
        Some(current.rawSnapshot)
      case _ =>
        historicJournalFileAfter(after)
          .map { historicJournalFile =>
            logger.debug(s"Reading snapshot from $historicJournalFile")
            historicJournalFile.eventReader.rawSnapshot
          }
    }

  /**
    * @return `Task(None)` torn, `after` < `tornEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]] = {
    val result = maybeCurrentEventReader match {
      case Some(current) if current.fileEventId <= after =>
        current.eventsAfter(after)
      case _ =>
        historicEventsAfter(after)
    }
    evictUnusedEventReaders()
    result
  }

  override def toString = s"JournalEventWatch(${journalMeta.name})"

  private def historicEventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]] =
    historicJournalFileAfter(after) flatMap { historicJournalFile =>
      var last = after
      historicJournalFile.eventReader.eventsAfter(after) map { events =>
        events.tapEach { stamped =>
          last = stamped.eventId
        } ++  // ++ is lazy, so last element will contain last read eventId
          (if (last == after)  // Nothing read
            CloseableIterator.empty
          else {  // Continue with next HistoricEventReader or CurrentEventReader
            logger.debug(
              s"Continue with next HistoricEventReader or CurrentEventReader, last=$last after=$after")
            assertThat(last > after, s"last=$last ≤ after=$after ?")
            eventsAfter(last) getOrElse CloseableIterator.empty  // Should never be torn here because last > after
          })
      }
    }

  /** Close unused HistoricEventReader. **/
  private def evictUnusedEventReaders(): Unit =
    fileEventIdToHistoric.values
      .filter(_.isEvictable)
      .toVector.sortBy(_.lastUsedAt)
      .dropRight(keepOpenCount)
      .foreach(_.evictEventReader())

  private def historicJournalFileAfter(after: EventId): Option[HistoricJournalFile] =
    fileEventIdToHistoric.values.toVector.reverseIterator.find(_.fileEventId <= after)

  def fileEventIds: Seq[EventId] =
    synchronized {
      fileEventIdToHistoric.keys.toSeq.sorted ++ maybeCurrentEventReader.map(_.fileEventId)
    }

  def journalPosition =
    checkedCurrentEventReader.map(_.journalPosition)

  def observeFile(journalPosition: JournalPosition,
    timeout: FiniteDuration, markEOF: Boolean, onlyAcks: Boolean)
  : Task[Checked[Observable[PositionAnd[ByteArray]]]] =
    Task.defer {
      import journalPosition.{fileEventId, position}

      if (onlyAcks && _isActiveNode)
        Task.pure(Left(Problem.pure(
          "Acknowledgements cannot be requested from an active cluster node (two active cluster nodes?)")))
      else
        announcedEventReaderPromise match {
          case Some((`fileEventId`, promise)) =>
            if (!promise.isCompleted) {
              logger.debug(s"observeFile($fileEventId): waiting for this new journal file")
            }
            Task.fromFuture(promise.future).map {
              case None =>
                Right(Observable.empty) // JournalEventWatch has been closed
              case Some(currentEventReader) =>
                Right(currentEventReader.observeFile(position, timeout, markEOF = markEOF, onlyAcks = onlyAcks))
            }

          case _ =>
            val maybeEventReader = maybeCurrentEventReader
              .filter(_.fileEventId == fileEventId)
              .orElse(fileEventIdToHistoric.get(fileEventId).map(_.eventReader))
            maybeEventReader match {
              case None =>
                logger.debug(s"observeFile($journalPosition): announcedEventReaderPromise=$announcedEventReaderPromise " +
                  s"maybeCurrentEventReader=$maybeCurrentEventReader " +
                  s"fileEventIdToHistoric=${fileEventIdToHistoric.keys.toVector.sorted.mkString(",")}")
                Task.pure(Left(Problem(s"Unknown journal file=$fileEventId")))
              case Some(eventReader) =>
                Task(Right(eventReader.observeFile(position, timeout, markEOF = markEOF, onlyAcks = onlyAcks)))
            }
        }
    }

  private def lastEventId =
    synchronized {
      maybeCurrentEventReader match {
        case Some(o) => o.lastEventId
        case None => fileEventIdToHistoric.keys.maxOption getOrElse EventId.BeforeFirst
      }
    }

  def journalInfo: JournalInfo =
    synchronized {
      JournalInfo(
        lastEventId = lastEventId,
        tornEventId = tornEventId,
        (fileEventIdToHistoric.values.view
          .map(h =>
            JournalPosition(h.fileEventId, Try(size(h.file)) getOrElse -1)
          ) ++
            maybeCurrentEventReader.map(_.journalPosition)
        ).toVector)
    }

  private final class HistoricJournalFile(
    val fileEventId: EventId,
    val file: Path,
    initialEventReader: Option[EventReader] = None)
  {
    private val _eventReader = AtomicAny[EventReader](initialEventReader.orNull)

    def closeAfterUse(): Unit =
      for (r <- Option(_eventReader.get())) r.closeAfterUse()

    def close(): Unit =
      for (r <- Option(_eventReader.get())) r.close()

    @tailrec
    def eventReader: EventReader =
      _eventReader.get() match {
        case null =>
          val r = new HistoricEventReader(journalMeta, journalIdOnce.orThrow,
            fileEventId = fileEventId, file, config)
          if (_eventReader.compareAndSet(null, r)) {
            logger.debug(s"Using $r")
            r
          } else {
            r.close()
            eventReader
          }
        case r => r
      }

    def evictEventReader(): Unit = {
      val reader = _eventReader.get()
      if (reader != null) {
        if (!reader.isInUse && _eventReader.compareAndSet(reader, null)) {  // Race condition, may be become in-use before compareAndSet
          logger.debug(s"Evict $reader lastUsedAt=${Timestamp.ofEpochMilli(reader.lastUsedAt)})")
          reader.closeAfterUse()
        }
      }
    }

    def lastUsedAt: Long =
      _eventReader.get() match {
        case null => 0L
        case reader => reader.lastUsedAt
      }

    def isEvictable: Boolean = {
      val reader = _eventReader.get()
      reader != null && !reader.isInUse
    }

    override def toString = "HistoricJournalFile:" + file.getFileName
  }

  private def checkedCurrentEventReader: Checked[CurrentEventReader] =
    maybeCurrentEventReader.toChecked(JournalFileIsNotReadyProblem(journalMeta.fileBase))
}

object JournalEventWatch
{
  private val logger = Logger(getClass)

  private case class JournalFileIsNotReadyProblem(file: Path) extends Problem.Coded {
    def arguments = Map("file" -> file.getFileName.toString)
  }

  val TestConfig = config"""
    js7.journal.watch.keep-open = 2
    js7.journal.watch.index-size = 100
    js7.journal.watch.index-factor = 10
    js7.journal.users-allowed-to-release-events = []
    js7.journal.release-events-delay = 0s
    js7.journal.remove-obsolete-files = true
    js7.monix.tailrecm-limit = 1000
    """
}
