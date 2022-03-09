package js7.base.monixutils

import cats.effect.Resource
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

final class MonixBaseJvmTest extends AsyncFreeSpec
{
  "Resource" - {
    "executeOn" in {
      Task.defer {
        val schedulerName = "MonixBaseTest-executeOn"
        val scheduler = Scheduler.singleThread(schedulerName)
        var releaseThreadName = ""
        Resource
          .make(
            acquire = Task { Thread.currentThread.getName })(
            release = _ => Task { releaseThreadName = Thread.currentThread.getName})
          .use(name => Task(
            assert(name startsWith schedulerName)))
          .map(_ => assert(releaseThreadName startsWith schedulerName))
          .executeOn(scheduler)
          .<*(Task(scheduler.shutdown()))
      }
      .runToFuture
    }
  }
}
