package js7.journal

import com.typesafe.scalalogging.Logger
import java.util.Locale.ROOT
import js7.base.time.ScalaTime.RichDuration
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, Stamped}
import js7.journal.JournalActor.Persist
import js7.journal.JournalLogging._
import js7.journal.configuration.JournalConf
import scala.collection.{IndexedSeqView, mutable}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

trait JournalLogging
{
  protected val conf: JournalConf
  protected val logger: Logger

  private val syncOrFlushString: String =
    if (!conf.syncOnCommit)
      "flush "
    else if (conf.simulateSync.isDefined)
      "~sync "
    else
      "sync  "
  private val ackSyncOrFlushString = syncOrFlushString.toUpperCase(ROOT)
  private val infoLoggableEventClasses = new SubclassCache(conf.infoLogEvents)
  private val sb = new StringBuilder

  protected final def logCommitted(persists: IndexedSeqView[Persist], ack: Boolean) =
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

  private def dropEmptyPersists(persists: IndexedSeqView[Persist]): IndexedSeqView[Persist]
  = {
    val dropLeft = persists.segmentLength(_.stampedSeq.isEmpty)
    val dropRight = persists.reverse.segmentLength(_.stampedSeq.isEmpty)
    persists.slice(dropLeft, persists.length - dropRight)
  }

  private def logPersists(persists: IndexedSeqView[Persist], committedAt: Deadline,
    isLoggable: Stamped[AnyKeyedEvent] => Boolean = _ => true)
    (body: (Frame, Stamped[AnyKeyedEvent]) => Unit)
  : Unit = {
    var index = 0
    for (persist <- persists) {
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
    sb.fillRight(6) {
      if (isLast && persist.isLastOfFlushedOrSynced) {
        sb.append(if (ack) ackSyncOrFlushString else syncOrFlushString)
      } else if (isFirst && persistIndex == 0 && persistCount >= 2) {
        sb.append(persistCount)  // Wrongly counts multiple isLastOfFlushedOrSynced (but only SnapshotTaken)
      } else if (nr == nextToLastEventNr && persistEventCount >= 10_000) {
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
      sb.fillRight(6) { sb.append(duration.msPretty) }
    } else if (nr == nextToLastEventNr) {
      sb.fillLeft(7) { sb.append(persistEventCount) }
    } else {
      sb.append("       ")
    }

    sb.append(transactionMarker)
    sb.append(stamped.eventId)
    sb.append(' ')
    sb.append(stamped.value.toString.truncateWithEllipsis(200, firstLineOnly = true))
    logger.trace(sb.toString)
  }

  private def infoLogPersist(frame: Frame, stamped: Stamped[AnyKeyedEvent])
  : Unit = {
    import frame._
    import stamped.value.{event, key}
    sb.clear()
    sb.append(persistMarker)
    sb.append("Event ")
    sb.append(transactionMarker)
    if (key != NoKey) {
      sb.append(key)
      sb.append(" <-: ")
    }
    sb.append(event.toShortString)
    logger.info(sb.toString)
  }
}

object JournalLogging
{
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

  private final case class Frame(persist: Persist,
    persistEventCount: Int, persistIndex: Int, persistCount: Int,
    committedAt: Deadline)
  {
    val nextToLastEventNr = persist.eventNumber + persistEventCount - 2
    val duration = committedAt - persist.since
    var nr = persist.eventNumber
    var isFirst = true
    var isLast = true

    def persistMarker: Char =
      if (persistCount == 1) ' '
      else if (isFirst & persistIndex == 0) '╭'  //'╮'
      else if (isLast & persistIndex == persistCount - 1) '╰'  //'╯'
      else if (isLast) '├'  //'┤'
      else '│'

    def transactionMarker: Char =
      if (!persist.isTransaction || persistEventCount == 1) ' '
      else if (isFirst) '⎧'
      else if (isLast) '⎩'
      else if (nr == nextToLastEventNr) '⎨'
      else '⎪'
  }
}
