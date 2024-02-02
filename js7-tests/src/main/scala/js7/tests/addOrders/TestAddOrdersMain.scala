package js7.tests.addOrders

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import js7.base.catsutils.OurApp
import js7.base.log.Logger
import js7.base.time.Stopwatch.durationAndPerSecondString
import scala.concurrent.duration.FiniteDuration

object TestAddOrdersMain extends OurApp:

  private val ClearLine = "\u001B[K"

  def run(args: List[String]): IO[ExitCode] =
    IO.defer:
      Logger.initialize("JS7 TestAddOrdersMain")

      given IORuntime = runtime

      if args.isEmpty || args.sameElements(Array("--help")) then
        println("Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD")
        IO.pure(ExitCode.Success)
      else
        val settings = Settings.parseArguments(args.toSeq)

        def logCurrentStatistics(statistics: Statistics) =
          // TODO Crash is not reported
          if statistics.totalOrderCount > 0 then
            print(s"\r${statistics.toLine}  $ClearLine")

        def logAddOrderDuration(duration: FiniteDuration) =
          print("\r" + ClearLine +
            durationAndPerSecondString(duration, settings.orderCount, "orders added") +
            ClearLine + "\n" + ClearLine)

        TestAddOrders.run(settings, logCurrentStatistics, logAddOrderDuration)
          .map:
            case Left(problem) =>
              println(s"\r$ClearLine")
              println(problem.toString)
              ExitCode.Error

            case Right(statistics) =>
              //print(s"$ClearLine\n$ClearLine\n$ClearLine")  // Left "main orders completed" lines
              println(s"\r$ClearLine")
              for line <- statistics.logLines do println(line + ClearLine)
              ExitCode.Success
