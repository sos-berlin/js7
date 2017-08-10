package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.shared.event.journal.StatisticsCounter._
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

  def countCommit(eventCount: Int): Unit = {
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

  override def toString =
    if (events == 0) ""
    else f"$events events, $commits commits ($flushes flushes, $syncs syncs)"

  def timingString =
    if (flushes == 0)
      ""
    else
      (t(flushNanos, flushes, "flush") ++ t(syncNanos, syncs, "sync")).mkString(", ") +
        ", " + FactorFormat.format(commits.toDouble / flushes) + " commits/flush)"

  def flushCount = flushes
}

object StatisticsCounter {
  private val FactorFormat = NumberFormat.getInstance(Locale.ROOT) sideEffect { _.setMaximumFractionDigits(1) }

  private def t(nanos: Long, n: Int, name: String): Option[String] =
    if (n == 0) None
    else Some(Duration.ofNanos(nanos / n).pretty + s"/$name")
}
