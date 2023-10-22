package js7.launcher

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.SetOnce
import js7.data.job.JobKey
import js7.data.order.{OrderId, Outcome}
import js7.data.value.NamedValues
import js7.launcher.OrderProcess.*
import monix.eval.{Fiber, IO}
import scala.concurrent.Promise

trait OrderProcess:
  protected def run: IO[Fiber[Outcome.Completed]]

  protected def onStarted(fiber: Fiber[Outcome.Completed]) = {}

  def cancel(immediately: Boolean): IO[Unit]

  protected def fiberCanceled: IO[Outcome.Failed] = IO.never

  /** Returns a IO for the started and running process. */
  final def start(orderId: OrderId, jobKey: JobKey, stdObservers: StdObservers)
  : IO[IO[Outcome.Completed]] =
    run.map { fiber =>
      onStarted(fiber)
      IO.race(fiber.join, fiberCanceled)
        .guarantee(stdObservers.stop)
        .map(_.fold(identity, identity))
        .onErrorHandle { t =>
          logger.warn(s"$orderId in $jobKey: ${t.toStringWithCauses}", t)
          Outcome.Failed.fromThrowable(t)
        }
    }


object OrderProcess:
  private val logger = Logger[this.type]

  def apply(run: IO[Outcome.Completed]): OrderProcess =
    new Simple(run)

  def succeeded(result: NamedValues = Map.empty): OrderProcess =
    new Simple(IO.pure(Outcome.Succeeded(result)))

  def fromCheckedOutcome(checkedOutcome: Checked[Outcome.Completed]): OrderProcess =
    OrderProcess(IO.pure(Outcome.Completed.fromChecked(checkedOutcome)))

  private final class Simple(io: IO[Outcome.Completed])
  extends OrderProcess.FiberCancelling:
    val run = io.start

    override def toString = "OrderProcess.Simple"

  final case class Failed(problem: Problem)
  extends OrderProcess:
    protected def run: IO[Fiber[Outcome.Completed]] =
      IO.pure(Fiber(
        IO.pure(Outcome.Failed.fromProblem(problem)),
        cancel = IO.unit))

    def cancel(immediately: Boolean) =
      IO.unit

  private val CanceledOutcome = Outcome.Failed(Some("Canceled"))

  trait FiberCancelling extends OrderProcess:
    private val fiberOnce = SetOnce[Fiber[Outcome.Completed]]
    private val canceledPromise = Promise[Unit]()
    override protected val fiberCanceled = IO.fromFuture(canceledPromise.future)
      .as(CanceledOutcome)

    override protected def onStarted(future: Fiber[Outcome.Completed]): Unit =
      super.onStarted(future)
      fiberOnce := future

    def cancel(immediately: Boolean): IO[Unit] =
      fiberOnce.orThrow.cancel
        .map(_ => canceledPromise.trySuccess(()))
