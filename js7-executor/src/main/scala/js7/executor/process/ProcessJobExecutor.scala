package js7.executor.process

import cats.syntax.all._
import java.util.Locale.ROOT
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.problem.Checked
import js7.data.job.{CommandLine, JobResource, ProcessExecutable}
import js7.data.order.Outcome
import js7.data.value.expression.scopes.LazyNamedValueScope
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NullValue, StringValue}
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
  import jobConf.jobKey

  final def start = Task.pure(Right(()))

  protected final def makeOrderProcess(processOrder: ProcessOrder, startProcess: StartProcess)
  : OrderProcess = {
    import processOrder.{order, scopeForEnv, scopeForJobResources}

    def evalJobResourceEnv(jobResource: JobResource): Checked[Map[String, String]] = {
      val jobResourceVariablesScope = LazyNamedValueScope(
        scopeForJobResources.evalLazilyNameToExpression(jobResource.variables))
      evalEnv(
        jobResource.env,
        scopeForEnv |+| jobResourceVariablesScope)
    }

    val checkedJobResourcesEnv: Checked[Map[String, String]] =
      processOrder.jobResources
        .reverse/*left overrides right*/
        .traverse(evalJobResourceEnv)
        .map(_.fold(Map.empty)(_ ++ _))

    val processDriver = new ProcessDriver(
      TaskConfiguration(jobKey, executable.toOutcome, startProcess.commandLine, executable.login,
        v1Compatible = v1Compatible),
      jobExecutorConf)

    new OrderProcess {
      def run = {
        ( for {
            jobResourcesEnv <- checkedJobResourcesEnv
            v1 <- v1Env(processOrder)
          } yield
            processDriver
              .processOrder(
                order.id,
                (v1.view ++ startProcess.env ++ jobResourcesEnv).toMap,
                processOrder.stdObservers)
              .guarantee(processDriver.terminate)
        ).valueOr(problem =>
          Task.pure(Outcome.Failed.fromProblem(problem)))
      }

      override def cancel(immediately: Boolean) =
        Task {
          processDriver.kill(if (immediately: Boolean) SIGKILL else SIGTERM)
        }
    }
  }

  private def v1Env(processOrder: ProcessOrder): Checked[Map[String, String]] =
    if (!v1Compatible)
      Right(Map.empty)
    else
      for (defaultArguments <- processOrder.checkedDefaultArguments) yield
        (defaultArguments.view ++
          processOrder.order.namedValues ++
          processOrder.workflow.defaultArguments)
        .map { case (k, v) => k -> v.toStringValue }
        .collect {
          case (name, Right(v)) => name -> v // ignore toStringValue errors (like ListValue)
        }
        .map { case (k, StringValue(v)) => (V1EnvPrefix + k.toUpperCase(ROOT)) -> v }
        .toMap

  protected final def evalEnv(nameToExpr: Map[String, Expression], scope: Scope)
  : Checked[Map[String, String]] =
    scope.evaluator.evalExpressionMap(nameToExpr)
      .flatMap(_
        .view
        .filter(_._2 != NullValue)
        .toVector.traverse { case (k, v) => v.toStringValueString.map(k -> _) })
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
