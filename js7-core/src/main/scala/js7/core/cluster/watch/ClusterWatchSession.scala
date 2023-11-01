package js7.core.cluster.watch

import js7.base.problem.{Checked, Problem}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.common.pekkohttp.web.session.Session
import js7.core.cluster.watch.ClusterWatchSession.*
import monix.eval.Task
import monix.execution.atomic.Atomic
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec

trait ClusterWatchSession extends Session
{
  private val inlay = new Inlay

  def withRequestId[A](requestId: Long)(task: Task[Checked[A]]): Task[Checked[A]] =
    inlay.withRequestId(requestId)(task)
}

object ClusterWatchSession
{
  @TestOnly private[cluster] def newInlay() = new Inlay

  private[cluster] final class Inlay private[ClusterWatchSession] {
    private val lastRequestId = Atomic(-1L)
    private val lock = AsyncLock("ClusterWatchSession")

    def withRequestId[A](requestId: Long)(task: Task[Checked[A]]): Task[Checked[A]] =
      lock.lock(Task(update(requestId)).flatMapT(_ => task))

    @tailrec def update(requestId: Long): Checked[Unit] = {
      val last = lastRequestId.get()
      if (last >= requestId)
        Left(OutdatedRequestProblem)
      else if (lastRequestId.compareAndSet(last, requestId))
        Checked.unit
      else
        update(requestId)
    }
  }

  val OutdatedRequestProblem = Problem.pure("Ignoring outdated request")
}
