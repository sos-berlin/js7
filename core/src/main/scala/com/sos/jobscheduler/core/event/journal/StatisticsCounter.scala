package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.StatisticsCounter._
import java.lang.System.nanoTime
import java.text.NumberFormat
import java.time.Duration
import java.util.Locale

/**
  * @author Joacim Zschimmer
  */
private[journal] final class StatisticsCounter {
  private var events = 0
  private var commits = 0
  private var flushes = 0
  private var flushNanos = 0L
  private var syncs = 0
  private var syncNanos = 0L

  def countEventsToBeCommitted(eventCount: Int): Unit =
    if (eventCount > 0) {  // Count only commits with events
      events += eventCount
      commits += 1
    }

  def beforeFlush(): Unit = {
    flushNanos -= nanoTime
  }

  def afterFlush(): Unit = {
    flushes += 1
    flushNanos += nanoTime
  }

  def beforeSync(): Unit = {
    syncNanos -= nanoTime
  }

  def afterSync(): Unit = {
    syncs += 1
    syncNanos += nanoTime
  }

  override def toString = infoString

  def infoString =
    if (events == 0) "no events"
    else f"$events events"

  def debugString =
    if (events == 0) "no events"
    else f"$events events, $commits commits ($flushes flushes, $syncs syncs)"

  def timingString =
    if (flushes == 0)
      ""
    else {
      val factorFormat = NumberFormat.getInstance(Locale.ROOT)  // Not thread-safe
      factorFormat.setMaximumFractionDigits(1)
      factorFormat.setGroupingUsed(false)  // For MacOS
      (t(flushNanos, flushes, "flush") ++ t(syncNanos, syncs, "sync")).mkString(", ") +
        ", " + factorFormat.format(commits.toDouble / flushes) + " commits/flush"
    }

  def flushCount = flushes
}

object StatisticsCounter {

  private def t(nanos: Long, n: Int, name: String): Option[String] =
    if (n == 0) None
    else Some(Duration.ofNanos(nanos / n).pretty + s"/$name")
}
