package js7.executor

import js7.base.utils.SetOnce
import js7.data.order.Outcome
import js7.executor.OrderProcess._
import monix.eval.{Fiber, Task}
import monix.execution.{CancelableFuture, Scheduler}
import scala.concurrent.Future
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

trait OrderProcess
{
  private val futureOnce = SetOnce[CancelableFuture[Outcome.Completed]]

  protected def run: Task[Fiber[Outcome.Completed]]

  final def future = futureOnce.orThrow

  def cancel(immediately: Boolean): Task[Unit] =
    Task { future.cancel() }

  final def start(stdObservers: StdObservers)(implicit s: Scheduler)
  : Task[Future[Outcome.Completed]] =
    run.map(fiber =>
      futureOnce := fiber
        .join
        .tapEval(_ => stdObservers.stop)
        .onCancelRaiseError(CanceledException)
        .materialize.map {
          case Failure(CanceledException) => Outcome.Failed(Some("Canceled"))
          case Failure(t) => Outcome.Failed.fromThrowable(t)
          case Success(o) => o
        }
        .runToFuture
    )
}

object OrderProcess
{
  def apply(run: Task[Outcome.Completed]): OrderProcess =
    new Simple(run)

  private final class Simple(task: Task[Outcome.Completed])
  extends OrderProcess
  {
    def run = task.start

    override def toString = "OrderProcess.Simple"
  }

  private object CanceledException extends NoStackTrace
}
