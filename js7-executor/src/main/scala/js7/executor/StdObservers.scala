package js7.executor

import js7.base.monixutils.TaskObserver
import monix.eval.Task
import monix.reactive.Observer

final case class StdObservers(
  out: Observer[String],
  err: Observer[String],
  charBufferSize: Int)
{
  val outTaskObserver = TaskObserver(out)
  val errTaskObserver = TaskObserver(err)

  def stop: Task[Unit] =
    Task.parZip2(outTaskObserver.complete, errTaskObserver.complete)
      .as(())
}
