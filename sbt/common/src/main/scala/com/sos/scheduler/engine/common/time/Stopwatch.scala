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

  def itemsPerSecondString(n: Int, item: String, items: String = "") = Stopwatch.itemsPerSecondString(duration, n, item, items)

  def elapsedMs: Long = duration.toMillis

  def duration: Duration = Duration.ofNanos(nanoTime() - start)

  override def toString = duration.pretty
}

object Stopwatch {
  private val logger = Logger(getClass)

  def measureTime[A](message: String)(body: ⇒ A): A = {
    val start = nanoTime()
    val result = body
    val duration = Duration.ofNanos(nanoTime() - start)
    logger.info(s"$message ${duration.pretty}")
    result
  }

  def measureTime(n: Int, itemName: String, linePrefix: String = "", warmUp: Int = 1)(body: ⇒ Unit): Result = {
    for (_ ← 1 to warmUp) body
    val start = nanoTime()
    for (_ ← 1 to n) body
    val duration = Duration.ofNanos(nanoTime() - start)
    val result = Result(n, itemName, duration)
    logger.info(s"$linePrefix$result")
    result
  }

  final case class Result(n: Int, itemName: String, totalDuration: Duration) {
    def singleDuration = totalDuration / n
    val nanos = totalDuration.toNanos
    def perSecond = if (nanos == 0) "∞" else if (n == 0) "?" else (n * 1000L*1000*1000 / nanos).toString
    override def toString =
      if (n == 0)
        s"0 $itemName"
      else
        s"$perSecond $itemName/s (${totalDuration.pretty}/$n = ${singleDuration.pretty})"
  }

  /**
    * Returns something like "2s/3000 items, 666µs/item, 1500 items/s"
    */
  def itemsPerSecondString(duration: Duration, n: Int, item: String, items: String = "") = {
    val nanos = duration.toNanos
    val perSecond = if (nanos == 0) "∞" else (n * 1000L * 1000 * 1000 / nanos).toString
    val plural = if (items.nonEmpty) items else if (item.nonEmpty && item.last.isLetter) s"${item}s" else item
    s"${duration.pretty}/$n $plural, ${(duration / n).pretty}/$item, $perSecond $plural/s"
  }
}
