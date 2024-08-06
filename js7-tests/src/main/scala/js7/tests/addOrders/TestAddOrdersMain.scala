package js7.tests.addOrders

import cats.effect.{ExitCode, IO}
import js7.base.service.Service
import js7.base.utils.ProgramTermination
import js7.common.system.startup.ServiceApp

object TestAddOrdersMain extends ServiceApp:

  def run(args: List[String]): IO[ExitCode] =
    if args.isEmpty || args.sameElements(Array("--help")) then
      IO:
        println:
          "Usage: testAddOrders --workflow=WORKFLOWPATH --count=1 --user=USER:PASSWORD"
        ExitCode.Success
    else
      runService(args, Settings.fromCommandLine):
        conf =>
          Service.simpleResource:
            TestAddOrders.run(conf, logToStdout = true).flatMap:
              case Left(problem) =>
                IO:
                  println(problem.toString)
                  ProgramTermination.Failure

              case Right(statistics) =>
                IO.pure(ProgramTermination.Success)
