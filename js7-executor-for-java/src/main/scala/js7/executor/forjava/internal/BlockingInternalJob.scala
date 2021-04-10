package js7.executor.forjava.internal

import io.vavr.control.{Either => VEither}
import java.io.{PrintWriter, Writer}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.utils.Lazy
import js7.data_for_java.common.JavaUtils.Void
import js7.data_for_java.order.JOutcome
import js7.executor.forjava.internal.BlockingInternalJob._
import js7.executor.internal.{InternalJob, InternalJobAdapter}

/** For non-asynchronous thread-blocking internal Jobs written in Java.
  * Constructor and methods are executed in (from call to call changing) threads
  * out of a thread pool for blocking execution. */
@InternalJobAdapter(classOf[BlockingInternalJobAdapter])
trait BlockingInternalJob
{
  /** Called once after construction and before any other call.*/
  @throws[Exception] @Nonnull
  final def start(): VEither[Problem, Void] =
    VEither.right(Void)

  /** Called only once after last `start` or `processOrder`.
    * <ul>
    *   <li>
    *     When the constructor has thrown an exception.
    *   <li>
    *     When `start` has thrown an exception or returned `Left`.
    *   <li>
    *     When the Job (or the surrounding workflow) is being destroyed.
    *   <li>
    *     When the Agent is terminating.
    * </ul>
    * */
  @throws[Exception]
  final def stop() = {}

  /** Process the order.
    * <p>
    * Executed in a seperate thread. */
  @throws[Exception] @Nonnull
  def processOrder(@Nonnull step: Step): JOutcome.Completed
}

object BlockingInternalJob
{
  final case class JobContext(asScala: InternalJob.JobContext)
  extends JavaJobContext

  final case class Step(asScala: InternalJob.Step, outWriter: Writer, errWriter: Writer)
  extends JavaJobStep
  {
    private lazy val outLazy = Lazy(new PrintWriter(outWriter, true))
    private lazy val errLazy = Lazy(new PrintWriter(errWriter, true))

    lazy val out: PrintWriter = outLazy()
    lazy val err: PrintWriter = errLazy()

    private[internal] def close(): Unit =
      try for (o <- outLazy) o.close()
      finally for (o <- errLazy) o.close()
  }
}
