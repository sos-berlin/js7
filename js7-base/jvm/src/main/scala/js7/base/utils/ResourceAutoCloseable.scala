package js7.base.utils

import cats.effect.Resource
import js7.base.thread.MonixBlocking.syntax.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.duration.*

final case class ResourceAutoCloseable[A](resource: Resource[Task, A], timeout: Duration)
  (implicit s: Scheduler, src: sourcecode.Enclosing)
  extends AutoCloseable
{
  private lazy val (value_, release) = resource.allocated.await(timeout)
  private val closed = Atomic(false)

  def value: A =
    value_

  def close(): Unit =
    if (!closed.getAndSet(true)) {
      release.await(timeout)
    }
}
