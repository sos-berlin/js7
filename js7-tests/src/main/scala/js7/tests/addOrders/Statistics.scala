package js7.tests.addOrders

import cats.kernel.Eq
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.*
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.*

private final case class Statistics(
  duration: FiniteDuration,
  lastOrderCount: Int,
  eventCount: Int,
  completedOrderCount: Int,
  failedOrderCount: Int,
  totalOrderDuration: FiniteDuration,
  maximumOrderDuration: FiniteDuration,
  completedForkedOrderCount: Int,
  processedCount: Int,
  totalProcessDuration: FiniteDuration,
  maximumProcessDuration: FiniteDuration,
  stdWritten: Long):

  def totalOrderCount = completedOrderCount + completedForkedOrderCount

  def toLine: String =
    completedOrderCount.toString + "+" + completedForkedOrderCount + " orders completed" +
      ((failedOrderCount > 0) ?? s", $failedOrderCount FAILED") + " · " +
      processedCount + " jobs executed · " +
      eventCount + " events · " +
      toKBGB(stdWritten) + " stdout+stderr · " +
      duration.pretty

  def logLines: Seq[String] =
    Seq(
      numberAndPerSecondString(duration, completedOrderCount, "main orders") + ", " +
        ((failedOrderCount > 0) ?? s", $failedOrderCount ORDERS FAILED"),
      numberAndPerSecondString(duration, totalOrderCount, "orders"),
      numberAndPerSecondString(duration, processedCount, "jobs"),
      numberAndPerSecondString(duration, eventCount, "events"),
      numberAndPerSecondString(duration, stdWritten / 1_000_000, "MB stdout+stderr"),
      s"∅ ${if completedOrderCount == 0 then "∞" else (totalOrderDuration / completedOrderCount).pretty} order duration",
      s"∅ ${if processedCount == 0 then "∞" else (totalProcessDuration / processedCount).pretty} job duration" +
        s", longest is ${maximumProcessDuration.pretty}",
      duration.pretty)

private object Statistics:
  implicit val eq: Eq[Statistics] = Eq.fromUniversalEquals
