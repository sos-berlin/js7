package js7.tests.addOrders

import cats.effect.{ExitCode, IO}
import js7.base.catsutils.OurApp
import js7.base.log.Logger

object TestAddOrdersMain extends OurApp:

  def run(args: List[String]): IO[ExitCode] =
    IO.defer:
      Logger.initialize("JS7 TestAddOrdersMain")

      if args.isEmpty || args.sameElements(Array("--help")) then
        IO:
          println:
            "Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD"
          ExitCode.Success
      else
        TestAddOrders.run(Settings.parseArguments(args), logToStdout = true)
          .flatMap:
            case Left(problem) =>
              IO:
                println(problem.toString)
                ExitCode.Error

            case Right(statistics) =>
              IO:
                ExitCode.Success
