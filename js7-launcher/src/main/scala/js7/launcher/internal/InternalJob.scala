package js7.launcher.internal

import java.nio.charset.Charset
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.monixutils.TaskObserver
import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichPartialFunction}
import js7.data.agent.AgentPath
import js7.data.job.{InternalExecutable, JobConf, JobResourcePath}
import js7.data.value.expression.Expression
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.launcher.{OrderProcess, ProcessOrder}
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import scala.collection.MapView
import scala.collection.immutable.ListMap
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

trait InternalJob
{
  protected type Step = InternalJob.Step

  def start: Task[Checked[Unit]] =
    Task.pure(Right(()))

  def stop: Task[Unit] =
    Task.unit

  def toOrderProcess(step: Step): OrderProcess
}

object InternalJob
{
  final case class JobContext(
    implementationClass: Class[?],
    executable: InternalExecutable,
    jobArguments: NamedValues,
    jobConf: JobConf,
    js7Scheduler: Scheduler,
    ioExecutor: IOExecutor,
    blockingJobScheduler: Scheduler,
    clock: AlarmClock,
    systemEncoding: Charset)
  {
    implicit def implicitJs7Scheduler: Scheduler = js7Scheduler
  }

  final case class Step private[internal](processOrder: ProcessOrder, arguments: NamedValues) {
    self =>

    lazy val jobResourceToVariables: ListMap[JobResourcePath, MapView[String, Checked[Value]]] =
      processOrder.jobResources.view
        .map(jr => jr.path -> processOrder.evalLazilyJobResourceVariables(jr))
        .to(ListMap)

    def env: Checked[Map[String, String]] =
      processOrder.checkedJobResourcesEnv

    def order = processOrder.order
    def workflow = processOrder.workflow
    def outObserver = processOrder.stdObservers.out
    def errObserver = processOrder.stdObservers.err
    def outTaskObserver = processOrder.stdObservers.outTaskObserver
    def errTaskObserver = processOrder.stdObservers.errTaskObserver

    def send(outErr: StdoutOrStderr, string: String): Task[Ack] =
      outErrTaskObserver(outErr).send(string)

    def outErrObserver(stdoutOrStderr: StdoutOrStderr): Observer[String] =
      stdoutOrStderr match {
        case Stdout => outObserver
        case Stderr => errObserver
      }

    private def outErrTaskObserver(outErr: StdoutOrStderr): TaskObserver[String] =
      outErr match {
        case Stdout => outTaskObserver
        case Stderr => errTaskObserver
      }

    def jobResourceVariable(jobResourcePath: JobResourcePath, variableName: String): Checked[Value] =
      jobResourceToVariables
        .rightOr(jobResourcePath, UnknownKeyProblem("JobResource", jobResourcePath.string))
        .flatMap(_.rightOr(variableName, UnknownKeyProblem("JobResource variable", variableName)))
        .flatten
  }

  abstract class Companion[I <: InternalJob](implicit classTag: ClassTag[I])
  {
    final def executable(
      script: String = "",
      jobArguments: Map[String, Expression] = Map.empty,
      arguments: Map[String, Expression] = Map.empty)
    : InternalExecutable =
      InternalExecutable(
        implicitClass[I].scalaName,
        script = script,
        jobArguments = jobArguments,
        arguments = arguments)

    final def execute(
      agentPath: AgentPath,
      arguments: Map[String, Expression] = Map.empty,
      subagentSelectionId: Option[Expression] = None,
      parallelism: Int = 1,
      timeout: Option[FiniteDuration] = None,
      jobResourcePaths: Seq[JobResourcePath] = Nil)
    : Execute.Anonymous =
      Execute(workflowJob(
        agentPath, arguments, subagentSelectionId,
        parallelism = parallelism,
        timeout = timeout,
        jobResourcePaths = jobResourcePaths))

    final def workflowJob(
      agentPath: AgentPath,
      arguments: Map[String, Expression] = Map.empty,
      subagentSelectionId: Option[Expression] = None,
      parallelism: Int = 1,
      timeout: Option[FiniteDuration] = None,
      jobResourcePaths: Seq[JobResourcePath] = Nil)
    : WorkflowJob =
      WorkflowJob(
        agentPath,
        executable(arguments = arguments),
        subagentSelectionId = subagentSelectionId,
        parallelism = parallelism,
        timeout = timeout,
        jobResourcePaths = jobResourcePaths)
  }
}
