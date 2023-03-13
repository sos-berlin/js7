package js7.base.utils

import cats.:<:
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.utils.AutoClosing.autoClosing
import monix.eval.Task
import monix.execution.Scheduler
import scala.annotation.unused
import scala.concurrent.duration.Duration

object AllocatedForJvm {
  implicit final class BlockingAllocated[F[_], A](private val allocated: Allocated[F, A])
  extends AnyVal {
    def blockingUse[R](stopTimeout: Duration)(body: A => R)
      (implicit s: Scheduler, @unused evidence: F :<: Task)
    : R = {
      val stop = allocated.stop.asInstanceOf[Task[Unit]]
      val ac: AutoCloseable = () =>
        stop
          .logWhenItTakesLonger(s"${allocated.toAllocatedString}.blockingUse.stop")
          .await(stopTimeout)

      autoClosing(ac)(_ => body(allocated.allocatedThing))
    }
  }
}