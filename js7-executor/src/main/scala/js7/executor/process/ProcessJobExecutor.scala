package js7.executor.process

import cats.implicits._
import java.util.Locale.ROOT
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.problem.Checked
import js7.data.job.{CommandLine, ProcessExecutable}
import js7.data.value.StringValue
import js7.data.value.expression.Evaluator
import js7.data.value.expression.Expression.ObjectExpression
import js7.executor.configuration.{JobExecutorConf, TaskConfiguration}
import js7.executor.internal.JobExecutor
import js7.executor.process.ProcessJobExecutor._
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task

trait ProcessJobExecutor extends JobExecutor
{
  protected val executable: ProcessExecutable
  protected def executorConf: JobExecutorConf

  import executable.v1Compatible
  import jobConf.{jobKey, workflowJob}

  final def start = Task.pure(Right(()))

  protected final def toOrderProcess(processOrder: ProcessOrder, startProcess: StartProcess): OrderProcess = {
    val taskRunner = executorConf.newTaskRunner(
      TaskConfiguration(jobKey, workflowJob.toOutcome, startProcess.commandLine,
        v1Compatible = v1Compatible))
    new OrderProcess {
      def run = taskRunner
        .processOrder(
          processOrder.order.id,
          v1Env(processOrder) ++ startProcess.env,
          processOrder.stdObservers)
        .guarantee(taskRunner.terminate)

      override def cancel(immediately: Boolean) =
        Task {
          taskRunner.kill(if (immediately: Boolean) SIGKILL else SIGTERM)
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

  protected final def evalEnv(evaluator: Evaluator, envExpr: ObjectExpression): Checked[Map[String, String]] =
    evaluator.evalObjectExpression(envExpr)
      .flatMap(_.nameToValue.toVector.traverse { case (k, v) => v.toStringValueString.map(k -> _) })
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
