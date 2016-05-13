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
    val perSecond = n * 1000 / totalDuration.toMillis
    override def toString = s"$perSecond $itemName/s (${totalDuration.pretty}/$n = ${singleDuration.pretty})"  }

  /**
    * Returns something like "2s/3000 items, 666µs/item, 1500 items/s"
    */
  def itemsPerSecondString(duration: Duration, n: Int, item: String, items: String = "") = {
    val plural = if (items.isEmpty) s"${item}s" else items
    s"${duration.pretty}/$n $plural, ${(duration / n).pretty}/$item, ${1000 * n / duration.toMillis} $plural/s"
  }
}
