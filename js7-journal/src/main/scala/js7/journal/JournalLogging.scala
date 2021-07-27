package js7.journal

import com.typesafe.scalalogging.Logger
import js7.base.time.ScalaTime.RichDuration
import js7.base.utils.ScalaUtils.syntax._
import js7.journal.JournalActor.LoggablePersist
import js7.journal.configuration.JournalConf
import scala.collection.View
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

trait JournalLogging
{
  protected def conf: JournalConf
  protected def logger: Logger

  private val syncOrFlushString: String =
    if (!conf.syncOnCommit)
      "flush "
    else if (conf.simulateSync.isDefined)
      "~sync "
    else
      "sync  "

  private val sb = new StringBuilder

  protected final def logCommitted(persists: Iterable[LoggablePersist], ack: Boolean) =
    logger.whenTraceEnabled {
      val committedAt = now
      var index = 0
      val loggablePersists = dropLastEmptyPersists(persists).toVector
      for (persist <- loggablePersists) {
        logPersist(persist, loggablePersists.size, index, committedAt, ack)
        index += 1
      }
    }

  private def dropLastEmptyPersists(persists: Iterable[LoggablePersist]): View[LoggablePersist] = {
    var i, nonEmptyLength = 0
    for (o <- persists) {
      i += 1
      if (o.stampedSeq.nonEmpty) nonEmptyLength = i
    }
    persists.view take nonEmptyLength
  }

  private def logPersist(persist: LoggablePersist, persistCount: Int, persistIndex: Int,
    committedAt: Deadline, ack: Boolean)
  : Unit = {
    var nr = persist.eventNumber
    val n = persist.stampedSeq.size
    val penultimateNr = persist.eventNumber + n - 2
    val duration = committedAt - persist.since
    var isFirst = true
    val stampedIterator = persist.stampedSeq.iterator
    while (stampedIterator.hasNext) {
      sb.clear()
      val stamped = stampedIterator.next()
      val isLast = !stampedIterator.hasNext

      if (ack) { // cluster
        sb.append('+')
      }
      sb.fillRight(5) { sb.append(nr) }
      sb.append(
        if (persistCount == 1) ' '
        else if (isFirst & persistIndex == 0) '╮'
        else if (isLast & persistIndex == persistCount - 1) '╯'
        else if (isLast) '┤'
        else '│')

      sb.fillRight(6) {
        if (isLast && persist.isLastOfFlushedOrSynced) {
          sb.append(syncOrFlushString)
        } else if (isFirst && persistIndex == 0 && persistCount >= 2) {
          sb.append(persistCount)  // Wrongly counts multiple isLastOfFlushedOrSynced (but only SnapshotTaken)
        } else if (nr == penultimateNr && n >= 10_000) {
          val micros = duration.toMicros
          if (micros != 0) {
            val k = (1000.0 * n / micros).toInt
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
      } else if (nr == penultimateNr) {
        sb.fillLeft(7) { sb.append(n) }
      } else {
        sb.append("       ")
      }

      sb.append(
        if (!persist.isTransaction || n == 1) ' '
        else if (isFirst) '⎧'
        else if (isLast) '⎩'
        else if (nr == penultimateNr) '⎨'
        else '⎪')
      sb.append(stamped.eventId)
      sb.append(' ')
      sb.append(stamped.value.toString.takeWhile(_ != '\n').truncateWithEllipsis(200))
      logger.trace(sb.toString())
      nr += 1
      isFirst = false
    }
  }
}
