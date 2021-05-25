package js7.executor

import js7.base.monixutils.TaskObserver
import js7.base.utils.ScalaUtils.syntax._
import js7.executor.utils.KeepLastLineObserver
import monix.eval.Task
import monix.reactive.Observer

final class StdObservers(
  val out: Observer[String],
  err_ : Observer[String],
  val charBufferSize: Int,
  keepLastErrLine: Boolean)
{
  private val lastLineErr = keepLastErrLine ? new KeepLastLineObserver(err_)
  val err: Observer[String] = lastLineErr.getOrElse(err_)

  def errorLine: Option[String] =
    lastLineErr.flatMap(_.lastLine)

  val outTaskObserver = TaskObserver(out)
  val errTaskObserver = TaskObserver(err)

  val stop: Task[Unit] =
    Task.parZip2(outTaskObserver.complete, errTaskObserver.complete)
      .void
      .memoize
}
