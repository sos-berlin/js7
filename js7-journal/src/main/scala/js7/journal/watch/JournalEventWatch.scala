package js7.journal.watch

import cats.syntax.semigroup._
import com.typesafe.config.Config
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{delete, exists, size}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.configutils.Configs._
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.{CheckedOption, Ops}
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{CloseableIterator, SetOnce}
import js7.common.jsonseq.PositionAnd
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournalInfo, JournalPosition, KeyedEvent, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles
import js7.journal.watch.JournalEventWatch._
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
  announceNextJournalFileEventId: Option[EventId] = None)
extends AutoCloseable
with RealEventWatch
with FileEventWatch
with JournalingObserver
{
  private val keepOpenCount = config.getInt("js7.journal.watch.keep-open")
  private val releaseEventsDelay = config.getDuration("js7.journal.release-events-delay").toFiniteDuration max 0.s
  // Read journal file names from directory while constructing

  @volatile private var afterEventIdToHistoric: SortedMap[EventId, HistoricJournalFile] =
    SortedMap.empty[EventId, HistoricJournalFile] ++
      JournalFiles.listJournalFiles(journalMeta.fileBase)
        .map(o => new HistoricJournalFile(o.afterEventId, o.file))
        .toKeyedMap(_.afterEventId)

  private val journalIdOnce = SetOnce.fromOption[JournalId](
    afterEventIdToHistoric
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
  private var currentEventReaderOption: Option[CurrentEventReader] = None

  // announceNextJournalFileEventId, the recovered EventId, optionally announces the next journal file
  // to avoid "Unknown journal file" if PassiveClusterNode starts replication of the next
  // journal file before this active node call onJournalingStarted.
  // This may happen especially when the node starts with a big snapshot
  // which delays onJournalingStarted.
  @volatile
  private var nextEventReaderPromise: Option[(EventId, Promise[Option[CurrentEventReader]])] =
    announceNextJournalFileEventId.map(_ -> Promise())

  @volatile
  private var _isActiveNode = false

  def close() = {
    afterEventIdToHistoric.values foreach (_.close())
    currentEventReaderOption foreach (_.close())
    for (o <- nextEventReaderPromise)
      o._2.trySuccess(None)
  }

  override def whenStarted = startedPromise.future

  def isActiveNode = _isActiveNode

  /*protected[journal]*/ def onJournalingStarted(
    file: Path,
    expectedJournalId: JournalId,
    tornLengthAndEventId: PositionAnd[EventId],
    flushedLengthAndEventId: PositionAnd[EventId],
    isActiveNode: Boolean)
  : Unit = {
    // Always tornLengthAndEventId == flushedLengthAndEventId ???
    logger.debug(s"onJournalingStarted ${file.getFileName}, torn length=${tornLengthAndEventId.position}, " +
      s"torn eventId=${tornLengthAndEventId.value}, flushed length=${flushedLengthAndEventId.position}, " +
      s"flushed eventId=${flushedLengthAndEventId.value}")
    journalIdOnce.toOption match {
      case None => journalIdOnce := expectedJournalId
      case Some(o) => require(expectedJournalId == o, s"JournalId $o does not match expected $expectedJournalId")
        //throw JournalIdMismatchProblem(journalMeta.fileBase, expectedJournalId = expectedJournalId, o).throwable
    }
    synchronized {
      _isActiveNode = isActiveNode
      val after = flushedLengthAndEventId.value
      if (after < lastEventId) throw new IllegalArgumentException(s"Invalid onJournalingStarted(after=$after), must be ≥ $lastEventId")
      for (current <- currentEventReaderOption) {
        if (file == current.journalFile) sys.error(s"onJournalingStarted: file == current.journalFile == ${file.getFileName}")
        if (current.lastEventId != tornLengthAndEventId.value)
          throw new IllegalArgumentException(s"onJournalingStarted(${tornLengthAndEventId.value}) does not match lastEventId=${current.lastEventId}")
        for (o <- afterEventIdToHistoric.get(current.tornEventId)) {
          o.closeAfterUse()  // In case last journal file had no events (and `after` remains), we exchange it
        }
        afterEventIdToHistoric += current.tornEventId -> new HistoricJournalFile(
          afterEventId = current.tornEventId,
          current.journalFile,
          Some(current)/*Reuse built-up JournalIndex*/)
      }
      val currentEventReader = new CurrentEventReader(journalMeta, expectedJournalId,
        tornLengthAndEventId, flushedLengthAndEventId, isActiveNode = isActiveNode, config)
      currentEventReaderOption = Some(currentEventReader)
      for ((eventId, promise) <- nextEventReaderPromise if eventId == tornLengthAndEventId.value)
        promise.success(Some(currentEventReader))
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
    for (o <- currentEventReaderOption) {
      logger.debug(s"onJournalingEnded ${o.journalFile.getFileName} fileLength=$fileLength")
      nextEventReaderPromise = Some(o.lastEventId -> Promise())
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
    val keepFileAfter = currentEventReaderOption match {
      case Some(current) if current.tornEventId <= untilEventId =>
        current.tornEventId
      case _ =>
        // Delete only journal files before the file containing `untilEventId`
        historicJournalFileAfter(untilEventId).fold(EventId.BeforeFirst)(_.afterEventId)
    }
    deleteJournalFiles(keepFileAfter)
    deleteGarbageFiles(keepFileAfter)
  }

  private def deleteJournalFiles(keepFileAfter: EventId): Unit = {
    val obsoletes = afterEventIdToHistoric.values.filter(_.afterEventId < keepFileAfter)
    for (historic <- obsoletes) {
      logger.info(s"Delete obsolete journal file '$historic' " +
        s"(${Try(toKBGB(size(historic.file))).fold(identity, identity)})")
      historic.close()
      try {
        delete(historic.file)
        synchronized {
          afterEventIdToHistoric -= historic.afterEventId
        }
      } catch {
        case e: IOException =>
          if (exists(historic.file)) {
            logger.warn(s"Cannot delete obsolete journal file '$historic': ${e.toStringWithCauses}")
          }
      }
    }
  }

  private def deleteGarbageFiles(untilEventId: EventId): Unit =
    for (file <- JournalFiles.listGarbageFiles(journalMeta.fileBase, untilEventId = untilEventId)) {
      logger.info(s"Delete garbage journal file '${file.getFileName}'")
      try delete(file)
      catch { case e: IOException =>
        if (exists(file)) {
          logger.warn(s"Cannot delete garbage journal file '$file': ${e.toStringWithCauses}")
        }
      }
    }

  def onFileWritten(flushedPosition: Long): Unit =
    for (o <- currentEventReaderOption) {
      o.onFileWritten(flushedPosition)
    }

  def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    checkedCurrentEventReader.orThrow.onEventsCommitted(positionAndEventId, n = n)
    onEventsCommitted(positionAndEventId.value)
  }

  def snapshotAfter(after: EventId) =
    rawSnapshotAfter(after)
      .map(_.mapParallelBatch()(_.parseJsonAs(journalMeta.snapshotJsonCodec).orThrow))

  def rawSnapshotAfter(after: EventId) =
    currentEventReaderOption match {
      case Some(current) if current.tornEventId <= after =>
        Some(current.rawSnapshot)
      case _ =>
        historicJournalFileAfter(after)
          .map { historicJournalFile =>
            logger.debug(s"Reading snapshot from journal file '$historicJournalFile'")
            historicJournalFile.eventReader.rawSnapshot
          }
    }

  /**
    * @return `Task(None)` torn, `after` < `tornEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]] = {
    val result = currentEventReaderOption match {
      case Some(current) if current.tornEventId <= after =>
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
            logger.debug(s"Continue with next HistoricEventReader or CurrentEventReader, last=$last after=$after")
            assertThat(last > after, s"last=$last ≤ after=$after ?")
            eventsAfter(last) getOrElse CloseableIterator.empty  // Should never be torn here because last > after
          })
      }
    }

  /** Close unused HistoricEventReader. **/
  private def evictUnusedEventReaders(): Unit =
    afterEventIdToHistoric.values
      .filter(_.isEvictable)
      .toVector.sortBy(_.lastUsedAt)
      .dropRight(keepOpenCount)
      .foreach(_.evictEventReader())

  private def historicJournalFileAfter(after: EventId): Option[HistoricJournalFile] =
    afterEventIdToHistoric.values.toVector.reverseIterator.find(_.afterEventId <= after)

  def fileEventIds: Seq[EventId] =
    synchronized {
      afterEventIdToHistoric.keys.toSeq.sorted ++ currentEventReaderOption.map(_.tornEventId)
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
        nextEventReaderPromise match {
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
            val maybeEventReader = currentEventReaderOption
              .filter(_.tornEventId == fileEventId)
              .orElse(afterEventIdToHistoric.get(fileEventId).map(_.eventReader))
            maybeEventReader match {
              case None =>
                logger.debug(s"observeFile($journalPosition): nextEventReaderPromise=$nextEventReaderPromise " +
                  s"afterEventIdToHistoric=${ afterEventIdToHistoric.keys.mkString(",") }")
                Task.pure(Left(Problem(s"Unknown journal file=$fileEventId")))
              case Some(eventReader) =>
                Task(Right(eventReader.observeFile(position, timeout, markEOF = markEOF, onlyAcks = onlyAcks)))
            }
        }
    }

  private def lastEventId =
    synchronized {
      currentEventReaderOption match {
        case Some(o) => o.lastEventId
        case None => afterEventIdToHistoric.keys.maxOption getOrElse EventId.BeforeFirst
      }
    }

  def journalInfo: JournalInfo =
    synchronized {
      JournalInfo(
        lastEventId = lastEventId,
        tornEventId = tornEventId,
        (afterEventIdToHistoric.values.view
          .map(h =>
            JournalPosition(h.afterEventId, Try(size(h.file)) getOrElse -1)
          ) ++
            currentEventReaderOption.map(_.journalPosition)
        ).toVector)
    }

  private final class HistoricJournalFile(
    val afterEventId: EventId,
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
          val r = new HistoricEventReader(journalMeta, journalIdOnce.orThrow, tornEventId = afterEventId, file, config)
          if (_eventReader.compareAndSet(null, r)) {
            logger.debug(s"Using HistoricEventReader(${file.getFileName})")
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
          logger.debug(s"Evict HistoricEventReader(${file.getFileName}' lastUsedAt=${Timestamp.ofEpochMilli(reader.lastUsedAt)})")
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

    override def toString = file.getFileName.toString
  }

  private def checkedCurrentEventReader: Checked[CurrentEventReader] =
    currentEventReaderOption.toChecked(JournalFileIsNotReadyProblem(journalMeta.fileBase))
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
