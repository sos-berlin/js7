package js7.launcher

import js7.base.problem.{Checked, Problem}
import js7.base.utils.SetOnce
import js7.data.order.Outcome
import js7.data.value.NamedValues
import monix.eval.{Fiber, Task}
import scala.concurrent.Promise

trait OrderProcess:
  protected def run: Task[Fiber[Outcome.Completed]]

  protected def onStarted(fiber: Fiber[Outcome.Completed]) = {}

  def cancel(immediately: Boolean): Task[Unit]

  protected def fiberCanceled: Task[Outcome.Failed] = Task.never

  /** Returns a Task for the started and running process. */
  final def start(stdObservers: StdObservers): Task[Task[Outcome.Completed]] =
    run.map { fiber =>
      onStarted(fiber)
      Task.race(fiber.join, fiberCanceled)
        .guarantee(stdObservers.stop)
        .map(_.fold(identity, identity))
        .onErrorHandle(Outcome.Failed.fromThrowable)
    }


object OrderProcess:
  def apply(run: Task[Outcome.Completed]): OrderProcess =
    new Simple(run)

  def succeeded(result: NamedValues = Map.empty): OrderProcess =
    new Simple(Task.pure(Outcome.Succeeded(result)))

  def fromCheckedOutcome(checkedOutcome: Checked[Outcome.Completed]): OrderProcess =
    OrderProcess(Task.pure(Outcome.Completed.fromChecked(checkedOutcome)))

  private final class Simple(task: Task[Outcome.Completed])
  extends OrderProcess.FiberCancelling:
    val run = task.start

    override def toString = "OrderProcess.Simple"

  final case class Failed(problem: Problem)
  extends OrderProcess:
    protected def run: Task[Fiber[Outcome.Completed]] =
      Task.pure(Fiber(
        Task.pure(Outcome.Failed.fromProblem(problem)),
        cancel = Task.unit))

    def cancel(immediately: Boolean) =
      Task.unit

  private val CanceledOutcome = Outcome.Failed(Some("Canceled"))

  trait FiberCancelling extends OrderProcess:
    private val fiberOnce = SetOnce[Fiber[Outcome.Completed]]
    private val canceledPromise = Promise[Unit]()
    override protected val fiberCanceled = Task.fromFuture(canceledPromise.future)
      .as(CanceledOutcome)

    override protected def onStarted(future: Fiber[Outcome.Completed]): Unit =
      super.onStarted(future)
      fiberOnce := future

    def cancel(immediately: Boolean): Task[Unit] =
      fiberOnce.orThrow.cancel
        .map(_ => canceledPromise.trySuccess(()))
