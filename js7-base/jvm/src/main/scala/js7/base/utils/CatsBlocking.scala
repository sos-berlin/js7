package js7.base.utils

import cats.effect.Resource
import js7.base.utils.AutoClosing.autoClosing
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.*

object CatsBlocking
{
  implicit final class BlockingTaskResource[+A](private val resource: Resource[Task, A]) extends AnyVal {
    def blockingUse[R](timeout: Duration)(block: A => R)(implicit s: Scheduler): R =
      autoClosing(ResourceAutoCloseable(resource, timeout))(o =>
        block(o.value))
  }
}
