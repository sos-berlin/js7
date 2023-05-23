package js7.journal.watch

import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.memoryLeakLimitedObservableTailRecM
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.CloseableIterator
import js7.base.utils.Collections.implicits.RichIterator
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.InputStreamJsonSeqReader.JsonSeqFileClosedProblem
import js7.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import js7.common.utils.UntilNoneIterator
import js7.data.event.{Event, EventId, JournalId, JournalSeparators, KeyedEvent, Stamped}
import js7.journal.data.JournalLocation
import js7.journal.recover.JournalReader
import js7.journal.watch.EventReader.*
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
private[watch] trait EventReader
extends AutoCloseable
{
  /** `committedLength` does not grow if `isOwnJournalIndex`. */
  protected def journalLocation: JournalLocation
  protected def expectedJournalId: JournalId
  protected def isHistoric: Boolean
  protected def journalFile: Path
  protected def fileEventId: EventId
  protected def firstEventPosition: Long
  protected def isFlushedAfterPosition(position: Long): Boolean
  protected def committedLength: Long
  protected def isEOF(position: Long): Boolean
  protected def whenDataAvailableAfterPosition(position: Long, until: MonixDeadline): Task[Boolean]
  /** Must be constant if `isHistoric`. */
  protected def config: Config

  private lazy val logger = Logger.withPrefix[this.type](journalFile.getFileName.toString)
  protected lazy val journalIndex = new JournalIndex(PositionAnd(firstEventPosition, fileEventId),
    size = config.getInt("js7.journal.watch.index-size"))
  private lazy val journalIndexFactor = config.getInt("js7.journal.watch.index-factor")
  private lazy val limitTailRecM = config.getInt("js7.monix.tailrecm-limit")
  protected final lazy val iteratorPool = new FileEventIteratorPool(
    journalLocation, expectedJournalId, journalFile, fileEventId, () => committedLength)
  @volatile
  private var _closeAfterUse = false
  @volatile
  private var _lastUsed = 0L

  final def closeAfterUse(): Unit = {
    logger.debug("closeAfterUse")
    _closeAfterUse = true
    if (!isInUse) close()
  }

  final def close(): Unit =
    iteratorPool.close()

  /**
    * @return None if torn
    */
  final def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]] = {
    val indexPositionAndEventId = journalIndex.positionAndEventIdAfter(after)
    import indexPositionAndEventId.position
    val iterator = iteratorPool.borrowIterator()
    closeOnError(iterator) {
      val pos = iterator.position
      if (pos != position &&
        (pos < position || iterator.eventId > after/*No seek if skipToEventAfter works without seek*/))
      {
        logger.trace(s"seek $position (eventId=${indexPositionAndEventId.value}, for $after) ≠ " +
          s"iterator $pos (eventId=${iterator.eventId})")
        iterator.seek(indexPositionAndEventId)
      }
      val exists = iterator.skipToEventAfter(journalIndex, after) // May run very long (minutes for gigabyte journals) !!!
      if (!exists) {
        iteratorPool.returnIterator(iterator)
        None
      } else
        Some(new EventIterator(iterator, after))
    }
  }

  private final class EventIterator(iterator_ : FileEventIterator, after: EventId)
  extends CloseableIterator[Stamped[KeyedEvent[Event]]]
  {
    private val iteratorAtomic = AtomicAny(iterator_)
    private var eof = false
    private var _next: Stamped[KeyedEvent[Event]] = null

    def close() =
      synchronized {
        // May be called asynchronously (parallel to hasNext or next), as by Monix guarantee or bracket
        for (it <- Option(iteratorAtomic.getAndSet(null))) {
          logger.trace(s"EventIterator(after=$after) closed")
          iteratorPool.returnIterator(it)
          if (_closeAfterUse && !isInUse || iteratorPool.isClosed) {
            logger.debug(s"CloseableIterator.close _closeAfterUse: '${EventReader.this}'")
            EventReader.this.close()
          }
        }
      }

    def hasNext =
      synchronized {
        !eof && {  // Avoid exception in iterator in case of automatically closed iterator (closeAtEnd, for testing)
          iteratorAtomic.get() match {
            case null =>
              logger.debug(JsonSeqFileClosedProblem(iteratorName).toString)
              eof = true  // EOF to avoid exception logging (when closed (cancelled) asynchronously before hasNext, but not before `next`).
              false
            case iterator =>
              _next != null || {
                val has = iterator.hasNext
                eof |= !has
                if (has) {
                  // Read ahead next event to be sure to let the following `next()` succeed.
                  // So `close` may be executed asynchronously between `hasNext` and `next`.
                  // This may happen when the (HTTP) client stops reading.
                  _lastUsed = Timestamp.currentTimeMillis
                  val stamped = iterator.next()
                  assertThat(stamped.eventId >= after, s"${stamped.eventId} ≥ $after")
                  if (isHistoric) {
                    journalIndex.tryAddAfter(stamped.eventId, iterator.position)
                  }
                  _next = stamped
                } else {
                  if (isHistoric) {
                    journalIndex.freeze(journalIndexFactor)
                  }
                  close()
                }
                has
              }
          }
        }
      }

    def next() =
      synchronized {
        hasNext
        _next match {
          case null =>
            if (iteratorAtomic.get() == null) throw new ClosedException(iterator_.journalFile)
            throw new NoSuchElementException("EventReader read past end of file")
          case result =>
            _next = null
            result
        }
      }

    private def iteratorName = iterator_.toString
  }

  final def snapshot: Observable[Any] =
    JournalReader.snapshot(journalLocation.S, journalFile, expectedJournalId)

  final def rawSnapshot: Observable[ByteArray] =
    JournalReader.rawSnapshot(journalLocation.S, journalFile, expectedJournalId)

  /** Observes a journal file lines and length. */
  final def observeFile(position: Long, timeout: FiniteDuration, markEOF: Boolean = false, onlyAcks: Boolean)
  : Observable[PositionAnd[ByteArray]] =
    Observable.deferAction(implicit scheduler =>
      Observable.fromResource(InputStreamJsonSeqReader.resource(journalFile))
        .flatMap { jsonSeqReader =>
          val until = now + timeout
          jsonSeqReader.seek(position)

          memoryLeakLimitedObservableTailRecM(position, limit = limitTailRecM)(position =>
            Observable.fromTask(whenDataAvailableAfterPosition(position, until))
              .flatMap {
                case false =>  // Timeout
                  Observable.empty
                case true =>  // Data may be available
                  var lastPosition = position
                  var eof = false
                  var iterator = UntilNoneIterator {
                    val maybeLine = jsonSeqReader.readRaw().map(_.value)
                    eof = maybeLine.isEmpty
                    lastPosition = jsonSeqReader.position
                    maybeLine.map(PositionAnd(lastPosition, _))
                  }.takeWhileInclusive(_ => isFlushedAfterPosition(lastPosition))
                  if (onlyAcks) {
                    // TODO Optimierung: Bei onlyAcks interessiert nur die geschriebene Dateilänge.
                    //  Dann brauchen wir die Datei nicht zu lesen, sondern nur die geschriebene Dateilänge zurückzugeben.
                    var last = null.asInstanceOf[PositionAnd[ByteArray]]
                    iterator foreach { last = _ }
                    iterator = Option(last).iterator
                  }
                  iterator = iterator
                    .tapEach { o =>
                      if (o.value == EndOfJournalFileMarker) sys.error(s"Journal file must not contain a line like $o")
                    } ++
                      (eof && markEOF).thenIterator(PositionAnd(lastPosition, EndOfJournalFileMarker))
                  Observable.fromIteratorUnsafe(iterator map Right.apply) ++
                    Observable.fromIterable(
                      !eof ? Left(lastPosition))
                })
        })

  final def lastUsedAt: Long =
    _lastUsed

  final def isInUse = iteratorPool.isLent
}

object EventReader
{
  private val EndOfJournalFileMarker = JournalSeparators.EndOfJournalFileMarker.toByteArray
  final class TimeoutException private[EventReader] extends scala.concurrent.TimeoutException
}
