package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.shared.event.journal.StatisticsCounter._
import java.lang.System.nanoTime
import java.time.Duration

/**
  * @author Joacim Zschimmer
  */
private[journal] final class StatisticsCounter {
  private var commits = 0
  private var flushes = 0
  private var flushNanos = 0L
  private var syncs = 0
  private var syncNanos = 0L

  def countCommit(): Unit = {
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
    if (flushes == 0) ""
    else f"$commits commits ($flushes flushes, $syncs syncs)" //, coalescence factor ${commits.toDouble / flushes}%1.1f)"

  def timingString =
    if (flushes == 0) ""
    else (t(flushNanos, flushes, "flush") ++ t(syncNanos, syncs, "sync")) mkString ", "

  def flushCount = flushes
}

object StatisticsCounter {
  private def t(nanos: Long, n: Int, name: String): Option[String] =
    if (n == 0) None
    else Some(Duration.ofNanos(nanos / n).pretty + s"/$name")
}
