package js7.tests.addOrders

import js7.base.time.Stopwatch.durationAndPerSecondString
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.Futures.implicits.SuccessFuture
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.FiniteDuration

object TestAddOrdersMain
{
  private val ClearLine = "\u001B[K"

  def main(args: Array[String]): Unit = {
    coupleScribeWithSlf4j()

    if (args.isEmpty || args.sameElements(Array("--help"))) {
      println("Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD")
    } else {
      val settings = Settings.parseArguments(args.toSeq)

      def logCurrentStatistics(statistics: Statistics) =
        // TODO Crash is not reported
        if (statistics.totalOrderCount > 0) {
          print(s"\r${statistics.toLine}  $ClearLine")
        }

      def logAddOrderDuration(duration: FiniteDuration) =
        print("\r" + ClearLine +
          durationAndPerSecondString(duration, settings.orderCount, "orders added") +
          ClearLine + "\n" + ClearLine)

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
            println(s"\r$ClearLine")
            for (line <- statistics.logLines) println(line + ClearLine)
      }
    }
  }
}
