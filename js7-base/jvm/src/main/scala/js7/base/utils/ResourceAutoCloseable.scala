package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.utils.CatsUtils.syntax.RichResource
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.duration.*
import scala.util.Try

final case class ResourceAutoCloseable[A](resource: Resource[Task, A], timeout: Duration)
  (implicit aTag: Tag[A], s: Scheduler, src: sourcecode.Enclosing)
  extends AutoCloseable
{
  private lazy val allocated = Try(resource.toAllocated.await(timeout))
  private val closed = Atomic(false)

  def value: A =
    allocated.get.allocatedThing

  def close(): Unit =
    if (!closed.getAndSet(true)) {
      for (a <- allocated) a.release.await(timeout)
    }
}
