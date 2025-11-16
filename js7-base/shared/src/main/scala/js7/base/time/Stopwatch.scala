package js7.base.time

import java.lang.System.nanoTime
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*
import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class Stopwatch:
  private val start = nanoTime

  def result(n: Long, ops: String = "ops"): Result =
    Result(duration, n, ops)

  def itemsPerSecondString(n: Long, ops: String = "ops"): String =
    Stopwatch.itemsPerSecondString(duration, n, ops)

  def elapsedMs: Long =
    duration.toMillis

  def duration: FiniteDuration =
    (nanoTime - start).nanoseconds

  override def toString: String =
    duration.pretty


object Stopwatch:
  def measureTimeOfSingleRun(n: Int, ops: String = "ops")(body: => Unit): Result =
    Result(durationOf(body), n, ops)

  def durationOf(body: => Unit): FiniteDuration =
    val start = nanoTime
    body
    (nanoTime - start).nanoseconds

  def measureTime(n: Int, ops: String = "ops", warmUp: Int = 1)(body: => Unit): Result =
    try
      for _ <- 1 to warmUp do body
      val start = nanoTime
      for _ <- 1 to n do body
      val duration = (nanoTime - start).nanoseconds
      Result(duration, n, ops)
    catch { case NonFatal(t) =>
      t.addSuppressed(new RuntimeException(s"In measureTime($n, $ops)"))
      throw t
    }

  //def measureTimeParallel(n: Int, ops: String = "ops", warmUp: Int = 1)(body: => Unit)(implicit ec: ExecutionContext = ioRuntime.compue): Result = {
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
    if n < 10_000_000 then
      durationAndPerSecondString(duration, n / 1_000, "kB", gap = false)
    else
      durationAndPerSecondString(duration, n / 1_000_000, "MB")

  def durationAndPerSecondString(duration: FiniteDuration, n: Long, ops: String = "ops", gap: Boolean = true): String =
    Result(duration, n, ops, gap).toShortString

  def numberAndPerSecondString(duration: FiniteDuration, n: Long, ops: String = "ops", gap: Boolean = true): String =
    Result(duration, n, ops, gap).countAndPerSecondString

  def perSecondStringOnly(duration: FiniteDuration, n: Long, ops: String = "ops", gap: Boolean = true): String =
    Result(duration, n, ops, gap).toPerSecondsString

  private val decimalFormat: DecimalFormat =
    val symbols = new DecimalFormatSymbols(Locale.GERMANY)
    symbols.setDecimalSeparator('.')
    symbols.setGroupingSeparator('\'')
    new DecimalFormat("#,###.##", symbols)

  private def formatNumber(number: Long | Double): String =
    val n = number match
      case number: Long => BigDecimal.valueOf(number)
      case number: Double => BigDecimal.valueOf(number)
    if n < 0 then
      n.toString
    else if n < 10_000 then
      n.toString
    else if n == 1_000_000 then
      "million"
    else if n % 1_000_000 == 0 then
      s"${n / 1_000_000}m"
    else if n % 1000 == 0 then
      s"${n / 1000}k"
    else
      decimalFormat.format(n)

  final case class Result(
    duration: FiniteDuration,
    n: Long,
    ops: String = "ops",
    private val gap: Boolean = true):

    def singleDuration: FiniteDuration =
      duration / n

    def showPerSecond: Boolean =
      n >= 10 && duration >= 100.ms && singleDuration <= 1.s || n > 100_00

    def perSecondString: String =
      if duration.toNanos == 0 then
        "∞"
      else
        ((duration < 1.s) ?? "~") + formatNumber(n * 1000L*1000*1000 / duration.toNanos)

    private def gapOps = (gap ?? " ") + ops

    override def toString: String =
      n match
        case 0 => s"0$gapOps"
        case 1 => s"⏱️  ${duration.pretty}/$n$gapOps"
        case _ =>
          val suffix = showPerSecond ?? s", $perSecondString$gapOps/s"
          s"⏱️  ${duration.pretty}/${formatNumber(n)}$gapOps (⌀${singleDuration.pretty})$suffix"

    def toShortString: String =
      if n == 0 || !showPerSecond then
        s"${duration.pretty}/$n$gapOps"
      else
        s"${duration.pretty}/$n$gapOps, $perSecondString$gapOps/s"

    def countAndPerSecondString: String =
      if n == 0 then
        s"0$gapOps"
      else
        s"$n$gapOps, $perSecondString/s"

    def toPerSecondsString: String =
      if n == 0 then
        s"0$gapOps"
      else
        s"$perSecondString$gapOps/s"

  object Result:
    implicit def resultToString(result: Result): String =
      result.toString
