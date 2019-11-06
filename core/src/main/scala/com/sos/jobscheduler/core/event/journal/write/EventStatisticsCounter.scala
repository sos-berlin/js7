package com.sos.jobscheduler.core.event.journal.write

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import java.text.NumberFormat
import java.util.Locale

/**
  * @author Joacim Zschimmer
  */
private[journal] final class EventStatisticsCounter(initialEventCount: Int) extends StatisticsCounter
{
  private var events = initialEventCount
  private var commits = 0

  def countEventsToBeCommitted(eventCount: Int): Unit =
    if (eventCount > 0) {  // Count only commits with events
      events += eventCount
      commits += 1
    }

  override def toString =
    if (events == 0) "no events"
    else s"$events events" //+ (if (syncs > 0) s", $syncs syncs" else "")

  def debugString: Option[String] =
    (events != 0) ? (
      s"$events events, $commits commits ($flushesDebugString) " +
        (if (events == 0) ""
         else
          (if (stopwatch.duration >= 1.s) s"$flushesTimingString, " else "") + {
            val factorFormat = NumberFormat.getInstance(Locale.ROOT)  // Not thread-safe
            factorFormat.setMaximumFractionDigits(1)
            factorFormat.setGroupingUsed(false)  // For MacOS
            (if (flushCount > 0) factorFormat.format(commits.toDouble / flushCount) + s" commits/flush" else "") +
              (if (syncCount < 10) "" // syncOnCommit?
               else ", " +
                factorFormat.format(commits.toDouble / syncCount) + s" commits/sync, " +
                factorFormat.format(events.toDouble / syncCount) + s" events/sync")
          }))
}
