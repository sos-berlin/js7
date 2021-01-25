package js7.tests.addOrders

import cats.kernel.Eq
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch._
import scala.concurrent.duration._

private final case class Statistics(
  duration: FiniteDuration,
  lastOrderCount: Int,
  eventCount: Int,
  completedOrderCount: Int,
  totalOrderDuration: FiniteDuration,
  maximumOrderDuration: FiniteDuration,
  completedForkedOrderCount: Int,
  processedCount: Int,
  totalProcessDuration: FiniteDuration,
  maximumProcessDuration: FiniteDuration)
{
  def totalOrderCount = completedOrderCount + completedForkedOrderCount

  def logLines: Seq[String] =
    Seq(
      perSecondStringOnly(duration, completedOrderCount, "main orders") + ", " +
        perSecondStringOnly(duration, completedOrderCount + completedForkedOrderCount, "orders"),
      perSecondStringOnly(duration, totalOrderCount, "orders"),
      perSecondStringOnly(duration, processedCount, "processes"),
      s"∅ ${if (completedOrderCount == 0) "∞" else (totalOrderDuration / completedOrderCount).pretty} order duration",
      s"∅ ${if (processedCount == 0) "∞" else (totalProcessDuration / processedCount).pretty} process duration" +
        s", longest is ${maximumProcessDuration.pretty}",
      perSecondStringOnly(duration, eventCount, "events"))
      //duration.pretty)

  def completeOrdersString =
    s"$completedOrderCount+$completedForkedOrderCount main orders completed"

}

private object Statistics
{
  implicit val eq = Eq.fromUniversalEquals[Statistics]
}
