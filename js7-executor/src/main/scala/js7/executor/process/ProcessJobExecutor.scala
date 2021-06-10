package js7.executor.process

import cats.syntax.all._
import java.util.Locale.ROOT
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.problem.Checked
import js7.base.utils.Lazy
import js7.data.job.{CommandLine, ProcessExecutable}
import js7.data.order.Outcome
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{StringValue, Value}
import js7.executor.configuration.{JobExecutorConf, TaskConfiguration}
import js7.executor.internal.JobExecutor
import js7.executor.process.ProcessJobExecutor._
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task

trait ProcessJobExecutor extends JobExecutor
{
  protected val executable: ProcessExecutable
  protected def jobExecutorConf: JobExecutorConf

  import executable.v1Compatible
  import jobConf.{jobKey, workflowJob}

  final def start = Task.pure(Right(()))

  protected final def makeOrderProcess(processOrder: ProcessOrder, startProcess: StartProcess): OrderProcess = {
    import processOrder.order

    val checkedJobResourcesEnv: Checked[Map[String, String]] =
      checkedCurrentJobResources()
        .flatMap(_
          .reverse/*left overrides right*/
          .traverse { jobResource =>
            val lazyEvaluatedSettings: Map[String, Lazy[Checked[Value]]] =
              jobResource.settings.view
                .mapValues(expr => Lazy(processOrder.jobResourceScope.evaluator.eval(expr)))
                .toMap
            val scope = Scope.fromLazyNamedValues(lazyEvaluatedSettings) |+| processOrder.jobResourceScope
            jobResource.env.toSeq
              .traverse { case (k, v) => scope.evalString(v).map(k -> _) }
              .map(_.toMap)
          })
        .map(_.fold(Map.empty)(_ ++ _))

    val processDriver = new ProcessDriver(
      TaskConfiguration(jobKey, executable.toOutcome, startProcess.commandLine, executable.login,
        v1Compatible = v1Compatible),
      jobExecutorConf)

    new OrderProcess {
      def run =
        checkedJobResourcesEnv match {
          case Left(problem) =>
            Task.pure(Outcome.Failed.fromProblem(problem))

          case Right(jobResourcesEnv) =>
            processDriver
              .processOrder(
                order.id,
                (v1Env(processOrder).view ++ startProcess.env ++ jobResourcesEnv).toMap,
                processOrder.stdObservers)
              .guarantee(processDriver.terminate)
        }

      override def cancel(immediately: Boolean) =
        Task {
          processDriver.kill(if (immediately: Boolean) SIGKILL else SIGTERM)
        }
    }
  }

  private def v1Env(processOrder: ProcessOrder): Map[String, String] =
    if (!v1Compatible)
      Map.empty
    else
      (workflowJob.defaultArguments.view ++
        processOrder.defaultArguments ++
        processOrder.order.namedValues ++
        processOrder.workflow.defaultArguments
      ) .toMap
        .view
        .mapValues(_.toStringValue)
        .collect {
          case (name, Right(v)) => name -> v  // ignore toStringValue errors (like ListValue)
        }
        .map { case (k, StringValue(v)) => (V1EnvPrefix + k.toUpperCase(ROOT)) -> v }
        .toMap

  protected final def evalEnv(scope: Scope, nameToExpr: Map[String, Expression]): Checked[Map[String, String]] =
    scope.evaluator.evalExpressionMap(nameToExpr)
      .flatMap(_.toVector.traverse { case (k, v) => v.toStringValueString.map(k -> _) })
      .map(_.toMap)
}

object ProcessJobExecutor
{
  private val V1EnvPrefix = "SCHEDULER_PARAM_"

  private[process] final case class StartProcess(
    commandLine: CommandLine,
    name: String,
    env: Map[String, String])
  {
    override def toString = name
  }
}
