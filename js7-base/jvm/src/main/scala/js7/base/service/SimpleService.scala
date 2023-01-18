package js7.base.service

import cats.effect.Resource
import monix.eval.Task

trait SimpleService extends Service.StoppableByRequest
{
  protected def run: Task[Unit]

  protected def start =
    startService(Task
      .race(untilStopRequested, run)
      .map(_.fold(identity, identity)))

  override def toString = "SimpleService"
}

object SimpleService {
  def apply(run: Task[Unit]): SimpleService = {
    val run_ = run
    new SimpleService {
      protected def run = run_
    }
  }

  def resource(run: Task[Unit]): Resource[Task, SimpleService] =
    Service.resource(Task(SimpleService(run)))
}
