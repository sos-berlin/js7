package js7.executor

import js7.base.utils.SetOnce
import js7.data.order.Outcome
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}

trait OrderProcess
{
  private val futureOnce = SetOnce[CancelableFuture[Outcome.Completed]]

  protected def run: Task[Outcome.Completed]

  final def future = futureOnce.get

  def kill(immediately: Boolean) =
    future.cancel()

  final def runToFuture(implicit s: Scheduler): CancelableFuture[Outcome.Completed] =
    futureOnce getOrUpdate
      run.materialize.map(Outcome.Completed.fromTry).runToFuture
}

object OrderProcess
{
  def apply(run: Task[Outcome.Completed]) =
    new Simple(run)

  final class Simple(val run: Task[Outcome.Completed])
  extends OrderProcess

  private object CanceledException extends NoStackTrace
}
