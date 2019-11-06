package com.sos.jobscheduler.core.event.journal.write

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.event.journal.write.StatisticsCounter._
import java.lang.System.nanoTime
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[journal] trait StatisticsCounter
{
  final val stopwatch = new Stopwatch

  private var flushes = 0
  private var flushNanos = 0L
  private var syncs = 0
  private var syncNanos = 0L

  final def beforeFlush(): Unit =
    flushNanos -= nanoTime

  final def afterFlush(): Unit = {
    flushes += 1
    flushNanos += nanoTime
  }

  final def beforeSync(): Unit =
    syncNanos -= nanoTime

  final def afterSync(): Unit = {
    syncs += 1
    syncNanos += nanoTime
  }

  def flushesDebugString =
    s"$flushes flushes, $syncs syncs"

  protected def flushesTimingString =
    if (flushes == 0) ""
    else (t(flushNanos, flushes, "flush") ++ t(syncNanos, syncs, "sync")).mkString(", ")

  final def flushCount = flushes

  final def syncCount = syncs
}

object StatisticsCounter
{
  private def t(nanos: Long, n: Int, name: String): Option[String] =
    if (n == 0) None
    else Some((nanos / n).nanoseconds.pretty + s"/$name")
}
