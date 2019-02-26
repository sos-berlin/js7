package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.common.scalautil.Futures.implicits.RichFutures
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch._
import java.lang.System.nanoTime
import java.time.Duration
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
final class Stopwatch {
  private val start = nanoTime

  def result(n: Long, ops: String = "ops"): Result =
    Result(duration, n, ops)

  def itemsPerSecondString(n: Long, ops: String = "ops"): String =
    Stopwatch.itemsPerSecondString(duration, n, ops)

  def elapsedMs: Long =
    duration.toMillis

  def duration: Duration =
    Duration.ofNanos(nanoTime - start)

  override def toString =
    duration.pretty
}

object Stopwatch {
  def measureTime(n: Int, ops: String = "ops", warmUp: Int = 1)(body: => Unit): Result = {
    for (_ <- 1 to warmUp) body
    val start = nanoTime
    for (_ <- 1 to n) body
    val duration = Duration.ofNanos(nanoTime - start)
    Result(duration, n, ops)
  }

  def measureTimeParallel(n: Int, ops: String = "ops", warmUp: Int = 1)(body: => Unit)(implicit ec: ExecutionContext = ExecutionContext.global): Result = {
    (for (_ <- 1 to warmUp) yield Future { body }).awaitInfinite
    val start = nanoTime
    (for (_ <- 1 to n) yield Future { body }).awaitInfinite
    val duration = Duration.ofNanos(nanoTime - start)
    Result(duration, n, ops)
  }

  /**
    * Returns something like "2s/3000 items, 666µs/item, 1500 items/s"
    */
  def itemsPerSecondString(duration: Duration, n: Long, ops: String = "ops"): String =
    Result(duration, n, ops).toString

  final case class Result(duration: Duration, n: Long, ops: String = "ops") {
    def singleDuration = duration / n
    def perSecondString = if (duration.toNanos == 0) "∞" else (n * 1000L*1000*1000 / duration.toNanos).toString
    override def toString =
      if (n == 0)
        s"0 $ops"
      else
        s"${duration.pretty}/$n $ops (⌀${singleDuration.pretty}) $perSecondString $ops/s"
  }
}
