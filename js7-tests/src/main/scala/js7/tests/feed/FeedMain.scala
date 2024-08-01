package js7.tests.feed

import cats.effect.{ExitCode, IO, Resource}
import js7.agent.main.AgentMain.runService
import js7.base.service.Service
import js7.base.utils.ProgramTermination
import js7.common.system.startup.ServiceApp

object FeedMain extends ServiceApp:

  def run(args: List[String]): IO[ExitCode] =
    runService(args, "JS7 Feed", FeedConf.fromCommandLine):
      conf =>
        Service.simpleResource:
          if args.isEmpty || args.sameElements(Array("--help")) then
            IO:
              println:
                "Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD"
              ProgramTermination.Success
          else
            Feed.run(Resource.eval(IO.pure(System.in)), conf)
              .as(ProgramTermination.Success)
