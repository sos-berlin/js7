package js7.journal.log

import com.typesafe.scalalogging.Logger
import java.util.Locale.ROOT
import js7.base.log.CorrelId
import js7.base.time.ScalaTime._
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, Stamped}
import js7.journal.log.JournalLogger._
import scala.collection.{IndexedSeqView, mutable}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

private[journal] final class JournalLogger(
  syncOrFlushChars: String,
  infoLogEvents: Set[String],
  suppressTiming: Boolean = false)
{
  private val syncOrFlushCharsAndSpace = syncOrFlushChars + " "
  private val syncOrFlushWidth = 6 max syncOrFlushCharsAndSpace.length
  private val ackSyncOrFlushString = syncOrFlushCharsAndSpace.toUpperCase(ROOT)

  private val infoLoggableEventClasses = new SubclassCache(infoLogEvents)
  private val sb = new StringBuilder

  def logCommitted(persists: IndexedSeqView[Loggable], ack: Boolean = false): Unit =
    logger.whenInfoEnabled {
      val committedAt = now
      val myPersists = dropEmptyPersists(persists)

      logger.whenTraceEnabled {
        logPersists(myPersists, committedAt)(traceLogPersist(ack))
      }

      def isLoggable(stamped: Stamped[AnyKeyedEvent]) = {
        val event = stamped.value.event
        infoLoggableEventClasses.contains(event.getClass) || event.isFailed
      }

      val loggablePersists = myPersists.filter(_.stampedSeq.exists(isLoggable))
      if (loggablePersists.nonEmpty) {
        logPersists(loggablePersists.toVector.view, committedAt, isLoggable)(infoLogPersist)
      }
    }

  private def dropEmptyPersists(persists: IndexedSeqView[Loggable]): IndexedSeqView[Loggable] = {
    val dropLeft = persists.segmentLength(_.stampedSeq.isEmpty)
    val dropRight = persists.reverse.segmentLength(_.stampedSeq.isEmpty)
    persists.slice(dropLeft, persists.length - dropRight)
  }

  private def logPersists(
    persists: IndexedSeqView[Loggable],
    committedAt: Deadline,
    isLoggable: Stamped[AnyKeyedEvent] => Boolean = _ => true)
    (body: (Frame, Stamped[AnyKeyedEvent]) => Unit)
  : Unit =
    CorrelId.isolate { logCorrelId =>
      var index = 0
      for (persist <- persists) {
        logCorrelId := persist.correlId
        val stampedSeq = persist.stampedSeq.filter(isLoggable)
        val frame = Frame(persist, stampedSeq.length, index, persists.length, committedAt)
        val stampedIterator = stampedSeq.iterator
        var hasNext = stampedIterator.hasNext
        while (hasNext) {
          val stamped = stampedIterator.next()
          hasNext = stampedIterator.hasNext
          frame.isLast = !hasNext
          body(frame, stamped)
          frame.nr += 1
          frame.isFirst = false
        }
        index += 1
      }
    }

  private def traceLogPersist(ack: Boolean)(frame: Frame, stamped: Stamped[AnyKeyedEvent]): Unit = {
    import frame._
    sb.clear()
    sb.append(':')  // Something simple to grep
    //? sb.fillRight(5) { sb.append(nr) }
    sb.append(persistMarker)
    sb.fillRight(syncOrFlushWidth) {
      if (isLast && persist.isLastOfFlushedOrSynced) {
        sb.append(if (ack) ackSyncOrFlushString else syncOrFlushCharsAndSpace)
      } else if (isFirst && persistIndex == 0 && persistCount >= 2) {
        sb.append(persistCount)  // Wrongly counts multiple isLastOfFlushedOrSynced (but only SnapshotTaken)
      } else if (nr == beforeLastEventNr && persistEventCount >= 10_000) {
        val micros = duration.toMicros
        if (micros != 0) {
          val k = (1000.0 * persistEventCount / micros).toInt
          if (k < 1000) {
            sb.append(k)
            sb.append("k/s")
          } else {
            sb.append(k / 1000)
            sb.append("M/s")
          }
        }
      }
    }

    if (isLast) {
      sb.append(' ')
      sb.fillRight(6) {
        if (!suppressTiming && duration >= MinimumDuration) sb.append(duration.msPretty)
      }
    } else if (nr == beforeLastEventNr && beforeLastEventNr > persist.eventNumber) {
      sb.fillLeft(7) { sb.append(persistEventCount) }
    } else {
      sb.append("       ")
    }

    sb.append(transactionMarker(true))
    sb.append(stamped.eventId)
    sb.append(' ')
    sb.append(stamped.value.toString.truncateWithEllipsis(200, firstLineOnly = true))
    logger.trace(sb.toString)
  }

  private def infoLogPersist(frame: Frame, stamped: Stamped[AnyKeyedEvent]): Unit = {
    import frame._
    import stamped.value.{event, key}
    sb.clear()
    sb.append("Event ")
    sb.append(transactionMarker(false))
    if (key != NoKey) {
      sb.append(key)
      sb.append(" <-: ")
    }
    sb.append(event.toShortString)
    logger.info(sb.toString)
  }
}

object JournalLogger
{
  private val logger = Logger("js7.journal.Journal")
  private val MinimumDuration = 1.ms

  private[journal] trait Loggable {
    def correlId: CorrelId
    def eventNumber: Long
    def stampedSeq: Seq[Stamped[AnyKeyedEvent]]
    def isTransaction: Boolean
    def since: Deadline
    def isLastOfFlushedOrSynced: Boolean
  }

  final class SimpleLoggable(
    val correlId: CorrelId,
    val eventNumber: Long,
    val stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    val isTransaction: Boolean,
    val since: Deadline,
    val isLastOfFlushedOrSynced: Boolean)
  extends Loggable

  private final class SubclassCache(superclassNames: Set[String]) {
    private val cache = mutable.Map.empty[Class[_], Boolean]

    def contains(cls: Class[_ <: Event]) =
      cache.getOrElseUpdate(cls,
        superclassesOf(cls)
          .iterator
          .map(_.simpleScalaName)
          .exists(superclassNames.contains))

    override def toString = s"SubclassCache($superclassNames)"
  }

  private final case class Frame(persist: Loggable,
    persistEventCount: Int, persistIndex: Int, persistCount: Int,
    committedAt: Deadline)
  {
    val beforeLastEventNr = persist.eventNumber + persistEventCount - 2
    val duration = committedAt - persist.since
    var nr = persist.eventNumber
    var isFirst = true
    var isLast = true

    def persistMarker: Char =
      if (persistCount == 1)
        ' '
      else {
        // Return a bold nipple at start of a Persist
        if (isFirst)
          if (persistIndex == 0) '┍'
          else if (persistIndex == persistCount - 1 & isLast) '┕'  // Last Persist has one event
          else '┝'
        else if (isLast & persistIndex == persistCount - 1) '└'  // Last Persist has >1 events
        else '│'
      }

    def transactionMarker(forTrace: Boolean): Char = {
      if (persistEventCount == 1) ' '
      else if (persist.isTransaction)
        if (isFirst) '⎧'
        else if (isLast) '⎩'
        else if (forTrace && nr == beforeLastEventNr) '⎨'
        else '⎪'
      else if (forTrace) {
        if (isFirst) '┐'
        else if (isLast) '┘'
        //else if (nr == beforeLastEventNr) '┤'
        else '┆'
      } else ' '
    }
  }
}
