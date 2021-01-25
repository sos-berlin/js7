package js7.tests.addOrders

import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.perSecondString
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.Futures.implicits.SuccessFuture
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

final class TestAddOrdersMain {
}

object TestAddOrdersMain
{
  private val ClearLine = "\u001B[K"

  def main(args: Array[String]): Unit = {
    coupleScribeWithSlf4j()
    println("☀️  A blue sky is expected due to poor error handling")
    val settings = Settings.parseArguments(args.toSeq)

    def logCurrentStatistics(statistics: Statistics) =
      // TODO Crash is not reported
      if (statistics.totalOrderCount > 0) {
        print(s"\r${statistics.completeOrdersString}  $ClearLine")
      }

    def logAddOrderDuration(duration: FiniteDuration) =
      print("\r" + ClearLine +
        perSecondString(duration, settings.orderCount, "orders added") +
        ClearLine + "\n" + ClearLine)

    val since = now
    TestAddOrders.run(settings, logCurrentStatistics, logAddOrderDuration)
      .runToFuture
      .awaitInfinite
      match {
        case Left(problem) =>
          println(s"\r$ClearLine")
          println(problem.toString)
          System.exit(1)

        case Right(statistics) =>
          //print(s"$ClearLine\n$ClearLine\n$ClearLine")  // Left "main orders completed" lines
          println(s"\r${statistics.completeOrdersString}$ClearLine")
          for (line <- statistics.logLines) println(line + ClearLine)
          println(s"${since.elapsed.pretty} total$ClearLine")
    }
  }
}
