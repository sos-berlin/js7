package js7.tests.feed

import cats.effect.{ExitCode, IO, Resource}
import js7.base.catsutils.OurApp
import js7.common.system.startup.JavaMain

object FeedMain extends OurApp:

  def run(args: List[String]): IO[ExitCode] =
    JavaMain.run("JS7 Feed"):
      if args.isEmpty || args.sameElements(Array("--help")) then IO:
        println("Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD")
        ExitCode.Success
      else
        IO.defer:
          Feed.run(Resource.eval(IO.pure(System.in)), Settings.parseArguments(args))
            .as(ExitCode.Success)
