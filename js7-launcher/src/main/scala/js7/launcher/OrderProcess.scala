package js7.launcher

import cats.effect.{FiberIO, IO}
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.{NonFatalInterruptedException, SetOnce}
import js7.data.job.JobKey
import js7.data.order.{OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.launcher.OrderProcess.*

trait OrderProcess:

  protected def run: IO[OrderOutcome.Completed]

  protected[OrderProcess] def onStarted(fiber: FiberIO[OrderOutcome.Completed]): Unit = {}

  /** Cancel this process.
   * May be called before or after `onStarted` or `run`. */
  def cancel(immediately: Boolean): IO[Unit]

  /** Returns an IO for the started and running process. */
  final def start(orderId: OrderId, jobKey: JobKey): IO[FiberIO[OrderOutcome.Completed]] =
    run.start.flatMap: fiber =>
      onStarted(fiber)
      fiber
        .joinWith(onCancel = IO.pure(CanceledOutcome))
        .handleError: t =>
          val u = t match
            case t: NonFatalInterruptedException => t.getCause
            case _ => t
          logger.warn(s"$orderId in $jobKey: ${u.toStringWithCauses}", u)
          OrderOutcome.Failed.fromThrowable(u)
        .start


object OrderProcess:
  private val logger = Logger[this.type]

  def apply(run: IO[OrderOutcome.Completed]): OrderProcess =
    new Simple(run)

  def succeeded(result: NamedValues = Map.empty): OrderProcess =
    new Simple(IO.pure(OrderOutcome.Succeeded(result)))

  def fromCheckedOutcome(checkedOutcome: Checked[OrderOutcome.Completed]): OrderProcess =
    OrderProcess(IO.pure(OrderOutcome.Completed.fromChecked(checkedOutcome)))

  private final class Simple(
    protected val run: IO[OrderOutcome.Completed])
  extends OrderProcess.FiberCancelling:
    override def toString = "OrderProcess.Simple"

  final case class Failed(problem: Problem)
  extends OrderProcess:
    protected def run =
      IO.pure:
        OrderOutcome.Failed.fromProblem(problem)

    def cancel(immediately: Boolean): IO[Unit] =
      IO.unit

  private val CanceledOutcome = OrderOutcome.Failed(Some("Canceled"))

  trait FiberCancelling extends OrderProcess:
    private val fiberOnce = SetOnce[FiberIO[OrderOutcome.Completed]]

    override protected[OrderProcess] def onStarted(fiber: FiberIO[OrderOutcome.Completed]): Unit =
      super.onStarted(fiber)
      fiberOnce := fiber

    def cancel(immediately: Boolean): IO[Unit] =
      IO.defer:
        fiberOnce.orThrow.cancel
