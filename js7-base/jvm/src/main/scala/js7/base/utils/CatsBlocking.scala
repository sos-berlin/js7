package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.utils.AutoClosing.autoClosing
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

object CatsBlocking:

  implicit final class BlockingTaskResource[A](private val resource: Resource[Task, A]) extends AnyVal:
    @TestOnly
    def blockingUse[R](timeout: Duration)(block: A => R)
      (implicit aTag: Tag[A], s: Scheduler, src: sourcecode.Enclosing)
    : R =
      autoClosing(ResourceAutoCloseable(resource, timeout))(o =>
        block(o.value))
