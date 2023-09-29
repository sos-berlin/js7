package js7.journal.write

import java.lang.System.nanoTime
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.ScalaUtils.syntax.*
import js7.journal.write.StatisticsCounter.*
import scala.concurrent.duration.*

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
    (flushes > 0) ??
      (t(flushNanos, flushes, "flush") ++ t(syncNanos, syncs, "sync")).mkString(", ")

  final def flushCount = flushes

  final def syncCount = syncs
}

object StatisticsCounter
{
  private def t(nanos: Long, n: Int, name: String): Option[String] =
    if n == 0 then None
    else Some((nanos / n).nanoseconds.pretty + s"/$name")
}
