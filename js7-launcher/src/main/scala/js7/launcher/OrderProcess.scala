package js7.launcher

import js7.base.problem.Checked
import js7.base.utils.SetOnce
import js7.data.order.Outcome
import js7.data.value.NamedValues
import js7.launcher.OrderProcess._
import monix.eval.{Fiber, Task}
import monix.execution.{CancelableFuture, Scheduler}
import scala.concurrent.Future
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

trait OrderProcess
{
  protected def run: Task[Fiber[Outcome.Completed]]

  protected def onStarted(future: CancelableFuture[Outcome.Completed]) = {}

  def cancel(immediately: Boolean): Task[Unit]

  /** Returns a Task with a Future for the started and running process. */
  final def start(stdObservers: StdObservers)(implicit s: Scheduler)
  : Task[Future[Outcome.Completed]] =
    run.map { fiber =>
      val future = fiber
        .join
        .tapEval(_ => stdObservers.stop)
        .onCancelRaiseError(CanceledException)
        .materialize.map {
          case Failure(CanceledException) => Outcome.Failed(Some("Canceled"))
          case Failure(t) => Outcome.Failed.fromThrowable(t)
          case Success(o) => o
        }
        .runToFuture
      onStarted(future)
      future
    }

  // TODO def cancel(immediately: Boolean) here ?
}

object OrderProcess
{
  def apply(run: Task[Outcome.Completed]): OrderProcess =
    new Simple(run)

  def succeeded(result: NamedValues = Map.empty): OrderProcess =
    new Simple(Task.pure(Outcome.Succeeded(result)))

  def fromCheckedOutcome(checkedOutcome: Checked[Outcome.Completed]): OrderProcess =
    OrderProcess(Task.pure(Outcome.Completed.fromChecked(checkedOutcome)))

  private final class Simple(task: Task[Outcome.Completed])
  extends OrderProcess.FutureCancelling
  {
    def run =
      task.start

    override def toString =
      "OrderProcess.Simple"
  }

  trait FutureCancelling extends OrderProcess
  {
    private val futureOnce = SetOnce[CancelableFuture[Outcome.Completed]]

    private def future = futureOnce.orThrow

    override protected def onStarted(future: CancelableFuture[Outcome.Completed]): Unit = {
      super.onStarted(future)
      futureOnce := future
    }

    def cancel(immediately: Boolean): Task[Unit] =
      Task { future.cancel() }
  }

  private object CanceledException extends NoStackTrace
}
