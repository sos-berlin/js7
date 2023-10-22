package js7.base.monixutils

import cats.effect.Resource
import js7.base.test.OurAsyncTestSuite
import cats.effect.IO
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.traced

final class MonixBaseJvmTest extends OurAsyncTestSuite
{
  "Resource" - {
    "executeOn" in {
      IO.defer {
        val schedulerName = "MonixBaseTest-executeOn"
        val scheduler = Scheduler.singleThread(schedulerName)
        var releaseThreadName = ""
        Resource
          .make(
            acquire = IO { Thread.currentThread.getName })(
            release = _ => IO { releaseThreadName = Thread.currentThread.getName})
          .use(name => IO(
            assert(name startsWith schedulerName)))
          .map(_ => assert(releaseThreadName startsWith schedulerName))
          .executeOn(scheduler)
          .<*(IO(scheduler.shutdown()))
      }
      .runToFuture
    }
  }
}
