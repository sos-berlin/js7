package js7.base.time

import java.lang.System.nanoTime
import java.math.MathContext
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class Stopwatch:
  private val start = nanoTime

  def result(n: Long, ops: String = "ops"): Result =
    Result(duration, n, ops)

  def itemsPerSecondStringBold(n: Long, ops: String = "ops"): String =
    bold(itemsPerSecondString(n, ops))

  def itemsPerSecondString(n: Long, ops: String = "ops"): String =
    Stopwatch.itemsPerSecondString(duration, n, ops)

  def elapsedMs: Long =
    duration.toMillis

  def duration: FiniteDuration =
    (nanoTime - start).nanoseconds

  override def toString: String =
    duration.pretty


object Stopwatch:
  private val Zero = BigDecimal(0)
  private val One = BigDecimal(1)

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
      durationAndPerSecondString(duration, BigDecimal(n) / 1_000, "kB", gap = false)
    else if n < 1_000_000_000L then
      durationAndPerSecondString(duration, BigDecimal(n) / 1_000_000, "MB")
    else
      durationAndPerSecondString(duration, BigDecimal(n) / 1_000_000_000, "GB")

  def durationAndPerSecondString(duration: FiniteDuration, n: BigDecimal, ops: String = "ops", gap: Boolean = true): String =
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

  private def formatNumber(n: BigDecimal): String =
    if n < 0 then
      n.toString
    else if n < BigDecimal(995, 2) && !n.isWhole then
      n.setScale(1, HALF_UP).toString
    else if n < 10_000 then
      n.setScale(0, HALF_UP).toString
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
    n: BigDecimal,
    ops: String = "ops",
    private val gap: Boolean = true):

    private val roundedN: BigDecimal = round(n)

    private def round(n: BigDecimal): BigDecimal =
      if n.abs < 10 then n.round(MathContext(1)) else n.setScale(0, HALF_UP)

    def singleDuration: FiniteDuration =
      duration / n

    def showPerSecond: Boolean =
      n > 0 && duration >= 100.ms /*&& singleDuration <= 1.s*/ || n > 100_000

    def perSecondString: String =
      if duration.toNanos == 0 then
        "∞"
      else
        ((duration < 1.s) ?? "~") + formatNumber(n * 1_000_000_000 / duration.toNanos)

    private def gapOps = (gap ?? " ") + ops

    override def toString: String =
      n match
        case Zero => s"0$gapOps"
        case One => s"⏱️  ${duration.pretty}/$n$gapOps"
        case _ =>
          val suffix = showPerSecond ?? s", $perSecondString$gapOps/s"
          s"⏱️  ${duration.pretty}/${formatNumber(roundedN)}$gapOps (⌀${singleDuration.pretty})$suffix"

    def toShortString: String =
      if n == 0 || !showPerSecond then
        s"⏱️  ${duration.pretty}/$roundedN$gapOps"
      else
        s"⏱️  ${duration.pretty}/$roundedN$gapOps, $perSecondString$gapOps/s"

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
