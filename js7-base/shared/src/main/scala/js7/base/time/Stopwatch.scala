package js7.base.time

import java.lang.System.nanoTime
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch._
import js7.base.utils.ScalaUtils.syntax._
import scala.concurrent.duration._
import scala.language.implicitConversions

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

  def duration: FiniteDuration =
    (nanoTime - start).nanoseconds

  override def toString =
    duration.pretty
}

object Stopwatch
{
  def measureTimeOfSingleRun(n: Int, ops: String = "ops")(body: => Unit): Result =
    Result(durationOf(body), n, ops)

  def durationOf(body: => Unit): FiniteDuration = {
    val start = nanoTime
    body
    (nanoTime - start).nanoseconds
  }

  def measureTime(n: Int, ops: String = "ops", warmUp: Int = 1)(body: => Unit): Result = {
    for (_ <- 1 to warmUp) body
    val start = nanoTime
    for (_ <- 1 to n) body
    val duration = (nanoTime - start).nanoseconds
    Result(duration, n, ops)
  }

  //def measureTimeParallel(n: Int, ops: String = "ops", warmUp: Int = 1)(body: => Unit)(implicit ec: ExecutionContext = ExecutionContext.global): Result = {
  //  (for (_ <- 1 to warmUp) yield Future { body }).awaitInfinite
  //  val start = nanoTime
  //  (for (_ <- 1 to n) yield Future { body }).awaitInfinite
  //  val duration = (nanoTime - start).nanoseconds
  //  Result(duration, n, ops)
  //}

  /**
    * Returns something like "2s/3000 items, 666µs/item, 1500 items/s"
    */
  def itemsPerSecondString(duration: FiniteDuration, n: Long, ops: String = "ops"): String =
    Result(duration, n, ops).toString

  def bytesPerSecondString(duration: FiniteDuration, n: Long): String =
    if (n < 10_000_000)
      perSecondString(duration, n / 1_000, "kB", gap = false)
    else
      perSecondString(duration, n / 1_000_000, "MB")

  def perSecondString(duration: FiniteDuration, n: Long, ops: String = "ops", gap: Boolean = true): String =
    Result(duration, n, ops, gap).toShortString

  final case class Result(duration: FiniteDuration, n: Long, ops: String = "ops", private val gap: Boolean = true) {
    def singleDuration = duration / n
    def perSecondString = if (duration.toNanos == 0) "∞" else (n * 1000L*1000*1000 / duration.toNanos).toString
    val gapOps = (gap ?? " ") + ops

    override def toString =
      if (n == 0)
        s"0$gapOps"
      else
        s"${duration.pretty}/$n$gapOps (⌀${singleDuration.pretty}) $perSecondString$gapOps/s"

    def toShortString =
      if (n == 0)
        s"0$gapOps"
      else
        s"${duration.pretty}/$n$gapOps $perSecondString$gapOps/s"
  }
  object Result {
    implicit def resultToString(result: Result) = result.toString
  }
}
