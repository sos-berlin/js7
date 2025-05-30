package js7.tests.feed

import cats.effect.{ExitCode, IO, Resource}
import js7.base.service.SimpleMainService
import js7.common.system.startup.ServiceApp

object FeedMain extends ServiceApp:

  def run(args: List[String]): IO[ExitCode] =
    if args.isEmpty || args.sameElements(Array("--help")) then
      IO:
        println("Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD")
        ExitCode(0)
    else
      runService(args, FeedConf.fromCommandLine): conf =>
        programAsService:
          Feed.run(Resource.eval(IO.pure(System.in)), conf)
