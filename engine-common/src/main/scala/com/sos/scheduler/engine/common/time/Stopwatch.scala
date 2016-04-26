package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.lang.System.nanoTime
import java.time.Duration

/**
 * @author Joacim Zschimmer
 */
final class Stopwatch {
  private val start = nanoTime()

  def elapsedMs: Long = duration.toMillis

  def duration: Duration = Duration.ofNanos(nanoTime() - start)

  override def toString = duration.pretty
}

object Stopwatch {
  private val logger = Logger(getClass)

  def measureTime(n: Int, itemName: String)(body: ⇒ Unit): Result = {
    body  // Warm-up
    val start = nanoTime()
    for (_ ← 1 to n) body
    val duration = Duration.ofNanos(nanoTime() - start)
    val r = Result(n, itemName, duration)
    logger.info(r.toString)
    r
  }

  final case class Result(n: Int, itemName: String, totalDuration: Duration) {
    val singleDuration = totalDuration / n
    val nanos = totalDuration.toNanos
    val perSecond = if (nanos == 0) "∞" else (n * 1000L*1000*1000 / nanos).toString
    override def toString = s"$perSecond $itemName/s (${totalDuration.pretty}/$n = ${singleDuration.pretty})"  }
}
