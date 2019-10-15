package com.sos.jobscheduler.core.event.journal.watch

import akka.util.ByteString
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.Collections.implicits.RichIterator
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.http.AkkaHttpUtils.AkkaByteVector
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.UntilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.recover.JournalReader
import com.sos.jobscheduler.core.event.journal.watch.EventReader._
import com.sos.jobscheduler.core.problems.JsonSeqFileClosedProblem
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import com.typesafe.config.Config
import java.nio.file.Path
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
private[watch] trait EventReader[E <: Event]
extends AutoCloseable
{
  /** `committedLength` does not grow if `isOwnJournalIndex`. */
  protected def journalMeta: JournalMeta[E]
  protected def expectedJournalId: Option[JournalId]
  protected def isHistoric: Boolean
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long

  /** Must be constant if `isHistoric`. */
  protected def committedLength: Long

  /** EOF counts as data.
    * @return false on timeout */
  protected def whenDataAvailableAfterPosition(position: Long, until: Deadline): Task[Boolean]

  protected def isFlushedAfterPosition(position: Long): Boolean

  protected def isEOF(position: Long): Boolean

  protected def config: Config

  private lazy val logger = Logger.withPrefix[this.type](journalFile.getFileName.toString)
  protected lazy val journalIndex = new JournalIndex(PositionAnd(tornPosition, tornEventId),
    size = config.getInt("jobscheduler.journal.watch.index-size"))
  private lazy val journalIndexFactor = config.getInt("jobscheduler.journal.watch.index-factor")
  protected final lazy val iteratorPool = new FileEventIteratorPool(journalMeta, expectedJournalId, journalFile, tornEventId, () => committedLength)
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
  final def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] = {
    val indexPositionAndEventId = journalIndex.positionAndEventIdAfter(after)
    import indexPositionAndEventId.position
    val iterator = iteratorPool.borrowIterator()
    closeOnError(iterator) {
      if (iterator.position != position &&
        (iterator.position < position || iterator.eventId > after/*No seek if skipToEventAfter works without seek*/))
      {
        logger.trace(s"seek $position (eventId=${indexPositionAndEventId.value}, for $after) ≠ " +
          s"iterator ${iterator.position} (eventId=${iterator.eventId})")
        iterator.seek(indexPositionAndEventId)
      }
      val exists = iterator.skipToEventAfter(journalIndex, after) // May run very long (minutes for gigabyte journals) !!!
      if (!exists) {
        iteratorPool.returnIterator(iterator)
        None
      } else
        Some(new MyIterator(iterator, after))
    }
  }

  private final class MyIterator(iterator_ : FileEventIterator[E], after: EventId) extends CloseableIterator[Stamped[KeyedEvent[E]]] {
    private val iteratorAtomic = AtomicAny(iterator_)
    @volatile private var eof = false

    // May be called asynchronously (parallel to hasNext or next), as by Monix guarantee
    def close() =
      for (it <- Option(iteratorAtomic.getAndSet(null))) {
        iteratorPool.returnIterator(it)
        if (_closeAfterUse && !isInUse || iteratorPool.isClosed) {
          logger.debug(s"CloseableIterator.close _closeAfterUse: '${EventReader.this}'")
          EventReader.this.close()
        }
      }

    def hasNext =
      !eof && {  // Avoid exception in iterator in case of automatically closed iterator (closeAtEnd, for testing)
        iteratorAtomic.get match {
          case null =>
            logger.debug(JsonSeqFileClosedProblem(iteratorName).toString)
            eof = true  // EOF to avoid exception logging (when closed (canceled) asynchronously before hasNext, but not before `next`).
            false
          case iterator =>
            val has = iterator.hasNext
            eof |= !has
            if (!has && isHistoric) {
              journalIndex.freeze(journalIndexFactor)
            }
            if (!has) {
              close()
            }
            has
        }
      }

    def next() =
      iteratorAtomic.get match {
        case null => throw new ClosedException(iterator_.journalFile)
        case iterator =>
          _lastUsed = Timestamp.currentTimeMillis
          val stamped = iterator.next()
          assert(stamped.eventId >= after, s"${stamped.eventId} ≥ $after")
          if (isHistoric) {
            if (eof/*freezed*/) sys.error(s"FileEventIterator: !hasNext but next() returns a value, eventId=${stamped.eventId} position=${iterator.position}")
            journalIndex.tryAddAfter(stamped.eventId, iterator.position)
          }
          stamped
      }

    private def iteratorName = iterator_.toString
  }

  final def snapshotObjects: CloseableIterator[Any] =
    CloseableIterator.fromCloseable(new JournalReader(journalMeta, expectedJournalId, journalFile))(_.nextSnapshots())

  /** Observers a journal file lines and length. */
  final def observeFile(position: Long, timeout: FiniteDuration, markEOF: Boolean = false, onlyLastOfChunk: Boolean)
  : Observable[PositionAnd[ByteString]] =
    Observable.fromResource(InputStreamJsonSeqReader.resource(journalFile))
      .flatMap { jsonSeqReader =>
        val until = now + timeout
        jsonSeqReader.seek(position)

        Observable.tailRecM(position)(position =>
          Observable.fromTask(whenDataAvailableAfterPosition(position, until))
            .flatMap {
              case false =>  // Timeout
                Observable.empty
              case true =>  // Data may be available
                var lastPosition = position
                var eofMarked = false
                var iterator = UntilNoneIterator {
                  val maybeLine = jsonSeqReader.readRaw()
                  lastPosition = jsonSeqReader.position
                  maybeLine.map(PositionAnd(lastPosition, _))
                }
                iterator = iterator.takeWhileAndOne(_ => isFlushedAfterPosition(lastPosition))
                if (onlyLastOfChunk) {
                  // TODO Optimierung: Bei onlyLastOfChunk interessiert nur die geschriebene Dateilänge.
                  //  Dann brauchen wir die Datei nicht zu lesen, sondern nur die geschriebene Dateilänge zurückzugeben.
                  var last = null.asInstanceOf[PositionAnd[ByteString]]
                  iterator.foreach(last = _)
                  iterator = Option(last).iterator
                }
                if (markEOF && isEOF(lastPosition)) {
                  iterator = iterator
                    .map {
                      case o @ PositionAnd(_, EndOfJournalFileMarker) => sys.error(s"Journal file must not contain a line like $o")
                      case o => o
                    } ++ {
                      eofMarked = true
                      Iterator.single(PositionAnd(lastPosition, EndOfJournalFileMarker))
                    }
                }
                Observable.fromIteratorUnsafe(iterator map Right.apply) ++
                  Observable.fromIterable(
                    ((lastPosition > position/*something read*/ && !eofMarked) ?
                      Left(lastPosition)))  // continue tailRecM
              })
      }

  final def lastUsedAt: Long =
    _lastUsed

  final def isInUse = iteratorPool.isLent
}

object EventReader
{
  final class TimeoutException private[EventReader] extends scala.concurrent.TimeoutException
  private val EndOfJournalFileMarker = JournalSeparators.EndOfJournalFileMarker.toByteString
}
