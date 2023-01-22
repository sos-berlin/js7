package js7.base.service

import cats.effect.Resource
import monix.eval.Task

final class SimpleService private(
  protected val run: Task[Unit])
extends Service.StoppableByRequest
{
  protected def start =
    startService(Task
      .race(untilStopRequested, run)
      .map(_.fold(identity, identity)))

  override def toString = "SimpleService"
}

object SimpleService {
  def resource(run: Task[Unit]): Resource[Task, SimpleService] =
    Service.resource(Task(new SimpleService(run)))
}
