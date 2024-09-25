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

  protected[OrderProcess] def onStarted(fiber: FiberIO[OrderOutcome.Completed]): IO[Unit] =
    IO.unit

  /** Cancel this process.
   * May be called before or after `onStarted` or `run`. */
  def cancel(immediately: Boolean): IO[Unit]

  /** Returns an IO for the started and running process. */
  final def start(orderId: OrderId, jobKey: JobKey): IO[FiberIO[OrderOutcome.Completed]] =
    run.start.flatMap: fiber =>
      onStarted(fiber)
        .productR:
          fiber.joinWith(onCancel = IO.pure(CanceledOutcome))
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
    Simple(run)

  def cancelable(run: IO[OrderOutcome.Completed]): OrderProcess.FiberCancelable =
    Cancelable(run)

  def succeeded(result: NamedValues = Map.empty): OrderProcess =
    outcome(OrderOutcome.Succeeded(result))

  def checkedOutcome(checkedOutcome: Checked[OrderOutcome.Completed]): OrderProcess =
    outcome(OrderOutcome.Completed.fromChecked(checkedOutcome))

  def problem(problem: Problem): OrderProcess =
    outcome(OrderOutcome.Failed.fromProblem(problem))

  def outcome(outcome: OrderOutcome.Completed): OrderProcess =
    Simple(IO.pure(outcome))


  private final class Simple(protected val run: IO[OrderOutcome.Completed])
  extends OrderProcess:
    def cancel(immediately: Boolean) =
      IO:
        logger.warn(s"$toString: cancel method is not implemented")

    override def toString = "OrderProcess.Simple"


  final case class Failed(problem: Problem)
  extends OrderProcess:
    protected def run =
      IO.pure:
        OrderOutcome.Failed.fromProblem(problem)

    def cancel(immediately: Boolean): IO[Unit] =
      IO.unit

  private val CanceledOutcome = OrderOutcome.Failed(Some("Canceled"))


  trait FiberCancelable extends OrderProcess:
    private val fiberOnce = SetOnce[FiberIO[OrderOutcome.Completed]]

    override protected[OrderProcess] def onStarted(fiber: FiberIO[OrderOutcome.Completed]) =
      IO:
        fiberOnce := fiber

    def cancel(immediately: Boolean): IO[Unit] =
      IO.defer:
        fiberOnce.orThrow.cancel


  private final class Cancelable(protected val run: IO[OrderOutcome.Completed])
    extends OrderProcess.FiberCancelable:
    override def toString = "OrderProcess.Cancelable"
