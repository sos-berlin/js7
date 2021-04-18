package js7.executor

import js7.base.utils.SetOnce
import js7.data.order.Outcome
import js7.executor.OrderProcess._
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

trait OrderProcess
{
  private val futureOnce = SetOnce[CancelableFuture[Outcome.Completed]]

  protected def run: Task[Outcome.Completed]

  final def future = futureOnce.orThrow

  def cancel(immediately: Boolean): Task[Unit] =
    Task { future.cancel() }

  final def runToFuture(implicit s: Scheduler): CancelableFuture[Outcome.Completed] =
    futureOnce getOrUpdate
      run
        .onCancelRaiseError(CanceledException)
        .materialize.map {
          case Failure(CanceledException) => Outcome.Failed(Some("Canceled"))
          case Failure(t) => Outcome.Failed.fromThrowable(t)
          case Success(o) => o
        }
        .runToFuture
}

object OrderProcess
{
  def apply(run: Task[Outcome.Completed]) =
    new Simple(run)

  final class Simple(val run: Task[Outcome.Completed])
  extends OrderProcess

  private object CanceledException extends NoStackTrace
}
