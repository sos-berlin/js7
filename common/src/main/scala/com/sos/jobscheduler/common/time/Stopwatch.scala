package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import java.lang.System.nanoTime
import java.time.Duration
import Stopwatch._

/**
 * @author Joacim Zschimmer
 */
final class Stopwatch {
  private val start = nanoTime()

  def result(n: Int, ops: String = "ops"): Result =
    Result(duration, n, ops)

  def itemsPerSecondString(n: Int, ops: String = "ops"): String =
    Stopwatch.itemsPerSecondString(duration, n, ops)

  def elapsedMs: Long =
    duration.toMillis

  def duration: Duration =
    Duration.ofNanos(nanoTime() - start)

  override def toString =
    duration.pretty
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
    val result = Result(duration, n, itemName)
    logger.info(s"$linePrefix$result")
    result
  }

  /**
    * Returns something like "2s/3000 items, 666µs/item, 1500 items/s"
    */
  def itemsPerSecondString(duration: Duration, n: Int, ops: String = "ops"): String =
    Result(duration, n, ops).toString

  final case class Result(duration: Duration, n: Int, ops: String = "ops") {
    def singleDuration = duration / n
    def perSecondString = if (duration.toNanos == 0) "∞" else (n * 1000L*1000*1000 / duration.toNanos).toString
    override def toString =
      if (n == 0)
        s"0 $ops"
      else
        s"${duration.pretty}/$n $ops (${singleDuration.pretty}) $perSecondString $ops/s"
  }
}
