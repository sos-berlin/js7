package js7.launcher

import cats.effect.{FiberIO, IO}
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.SetOnce
import js7.data.job.JobKey
import js7.data.order.{OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.launcher.OrderProcess.*

trait OrderProcess:

  protected def run: IO[FiberIO[OrderOutcome.Completed]]

  protected def onStarted(fiber: FiberIO[OrderOutcome.Completed]) = {}

  def cancel(immediately: Boolean): IO[Unit]

  /** Returns an IO for the started and running process. */
  final def start(orderId: OrderId, jobKey: JobKey): IO[FiberIO[OrderOutcome.Completed]] =
    run.flatMap: fiber =>
      onStarted(fiber)
      fiber
        .joinWith(onCancel = IO.pure(CanceledOutcome))
        .handleError: t =>
          logger.warn(s"$orderId in $jobKey: ${t.toStringWithCauses}", t)
          OrderOutcome.Failed.fromThrowable(t)
        .start

object OrderProcess:
  private val logger = Logger[this.type]

  def apply(run: IO[OrderOutcome.Completed]): OrderProcess =
    new Simple(run)

  def succeeded(result: NamedValues = Map.empty): OrderProcess =
    new Simple(IO.pure(OrderOutcome.Succeeded(result)))

  def fromCheckedOutcome(checkedOutcome: Checked[OrderOutcome.Completed]): OrderProcess =
    OrderProcess(IO.pure(OrderOutcome.Completed.fromChecked(checkedOutcome)))

  private final class Simple(io: IO[OrderOutcome.Completed])
  extends OrderProcess.FiberCancelling:
    val run = io
      .start

    override def toString = "OrderProcess.Simple"

  final case class Failed(problem: Problem)
  extends OrderProcess:
    protected def run =
      IO.pure(OrderOutcome.Failed.fromProblem(problem): OrderOutcome.Completed).start

    def cancel(immediately: Boolean) =
      IO.unit

  private val CanceledOutcome = OrderOutcome.Failed(Some("Canceled"))

  trait FiberCancelling extends OrderProcess:
    private val fiberOnce = SetOnce[FiberIO[OrderOutcome.Completed]]

    override protected def onStarted(fiber: FiberIO[OrderOutcome.Completed]): Unit =
      super.onStarted(fiber)
      fiberOnce := fiber

    def cancel(immediately: Boolean): IO[Unit] =
      fiberOnce.orThrow.cancel
