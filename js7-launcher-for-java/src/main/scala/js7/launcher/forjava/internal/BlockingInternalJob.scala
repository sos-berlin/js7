package js7.launcher.forjava.internal

import io.vavr.control.{Either => VEither}
import java.io.{PrintWriter, Writer}
import java.util.{Map => JMap}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.utils.Lazy
import js7.data.job.JobResourcePath
import js7.data.value.Value
import js7.data_for_java.common.JavaUtils.Void
import js7.data_for_java.order.JOutcome
import js7.data_for_java.value.JExpression
import js7.data_for_java.vavr.VavrConverters._
import js7.launcher.forjava.internal.BlockingInternalJob._
import js7.launcher.internal.{InternalJob, InternalJobAdapter}
import monix.execution.Scheduler
import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters._

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

  /** Called only once after last `start` or `toOrderProcess`.
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

  /** Only returns a OrderProcess for a single order step.
    * <p>
    * Should not do anything else.
    * <p>
    * May be called multiple times in parallel.
    */
  @Nonnull
  def toOrderProcess(@Nonnull step: Step): OrderProcess
}

object BlockingInternalJob
{
  final case class JobContext(asScala: InternalJob.JobContext)
  extends JavaJobContext

  final case class Step(asScala: InternalJob.Step, outWriter: Writer, errWriter: Writer)
  extends JavaJobStep
  {
    private val outLazy = Lazy(new PrintWriter(outWriter, true))
    private val errLazy = Lazy(new PrintWriter(errWriter, true))

    lazy val out: PrintWriter = outLazy()
    lazy val err: PrintWriter = errLazy()

    private[internal] def close(): Unit =
      try for (o <- outLazy) o.close()
      finally for (o <- errLazy) o.close()

    def evalExpression(expression: JExpression): VEither[Problem, Value] =
      expression.asScala.eval(asScala.processOrder.scope)
        .toVavr

    def jobResourceToNameToCheckedValue: JMap[JobResourcePath, JMap[String, VEither[Problem, Value]]] =
      asScala.jobResourceToVariables
        .view.mapValues(_.mapValues(_.toVavr).toMap.asJava)
        .to(ListMap).asJava

    def jobResourceVariable(jobResourcePath: JobResourcePath, name: String): VEither[Problem, Value] =
      asScala.jobResourceVariable(jobResourcePath, name)
        .toVavr

    lazy val env: VEither[Problem, JMap[String, String]] =
      asScala.env.map(_.asJava).toVavr
  }
  object Step {
    def apply(asScala: InternalJob.Step)(implicit s: Scheduler): Step =
      Step(asScala, new ObserverWriter(asScala.outObserver), new ObserverWriter(asScala.errObserver))
  }

  trait OrderProcess
  {
    /** Process the order.
      * <p>
      * May be called multiple times in parallel.
      * <p>
      * Executed in a seperate thread. */
    @throws[Exception] @Nonnull
    def run(): JOutcome.Completed

    @throws[Exception] @Nonnull
    def cancel(immediately: Boolean) = {}
  }
}
