package js7.base.service

import cats.effect.Resource
import monix.eval.Task

final class CancelableService private(
  protected val run: Task[Unit])
extends Service.StoppableByRequest
{
  protected def start =
    startService(Task
      .race(untilStopRequested, run) // Cancels run
      .map(_.fold(identity, identity)))

  override def toString = "CancelableService"
}

object CancelableService {
  def resource(run: Task[Unit]): Resource[Task, CancelableService] =
    Service.resource(Task(new CancelableService(run)))
}
