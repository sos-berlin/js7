package js7.launcher.process

import cats.effect.{FiberIO, IO}
import java.util.Locale.ROOT
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.problem.Checked
import js7.data.job.{CommandLine, ProcessExecutable}
import js7.data.order.OrderOutcome
import js7.data.value.StringValue
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.launcher.process.ProcessJobLauncher.*
import js7.launcher.{OrderProcess, ProcessOrder}

trait ProcessJobLauncher extends JobLauncher:
  protected val executable: ProcessExecutable
  protected def jobLauncherConf: JobLauncherConf

  import executable.v1Compatible
  import jobConf.jobKey

  final val start = IO.pure(Checked.unit)

  protected final def makeOrderProcess(processOrder: ProcessOrder, startProcess: StartProcess)
  : OrderProcess =
    import processOrder.order

    val processDriver = new ProcessDriver(
      order.id,
      ProcessDriver.Conf(jobKey, executable.toOutcome, startProcess.commandLine, executable.login,
        v1Compatible = v1Compatible),
      jobLauncherConf)

    new OrderProcess:
      def run: IO[OrderOutcome.Completed] =
        val checkedEnv = for
          jobResourcesEnv <- processOrder.checkedJobResourcesEnv
          v1 <- v1Env(processOrder)
        yield (v1.view ++ startProcess.env ++ jobResourcesEnv).toMap
        checkedEnv match
          case Left(problem) =>
            IO.pure(OrderOutcome.Failed.fromProblem(problem))
          case Right(env) =>
            processDriver.runProcess(env, processOrder.stdObservers)

      def cancel(immediately: Boolean) =
        processDriver.kill(if immediately then SIGKILL else SIGTERM)

      override def toString = "ProcessJobLauncher.OrderProcess"

  private def v1Env(processOrder: ProcessOrder): Checked[Map[String, Some[String]]] =
    if !v1Compatible then
      Right(Map.empty)
    else
      import processOrder.{order, workflow}
      for defaultArguments <- processOrder.checkedJs1DefaultArguments yield
        (defaultArguments.view ++ order.v1CompatibleNamedValues(workflow))
          .map { case (k, v) => k -> v.toStringValue }
          .collect:
            case (name, Right(v)) => name -> v // ignore toStringValue errors (like ListValue)
          .map { case (k, StringValue(v)) => (V1EnvPrefix + k.toUpperCase(ROOT)) -> Some(v) }
          .toMap


object ProcessJobLauncher:
  private val V1EnvPrefix = "SCHEDULER_PARAM_"

  private[process] final case class StartProcess(
    commandLine: CommandLine,
    name: String,
    env: Map[String, Option[String]]):
    override def toString = name
