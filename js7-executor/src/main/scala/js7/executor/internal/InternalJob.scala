package js7.executor.internal

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
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import scala.collection.MapView
import scala.collection.immutable.ListMap
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
    implementationClass: Class[_],
    executable: InternalExecutable,
    jobArguments: NamedValues,
    jobConf: JobConf,
    implicit val js7Scheduler: Scheduler,
    ioExecutor: IOExecutor,
    blockingJobScheduler: Scheduler,
    clock: AlarmClock)

  final case class Step private(processOrder: ProcessOrder, arguments: NamedValues) {
    self =>

    lazy val jobResourceToVariables: ListMap[JobResourcePath, MapView[String, Checked[Value]]] =
      processOrder.jobResources.view
        .map(jr => jr.path -> processOrder.evalLazilyJobResourceVariables(jr))
        .to(ListMap)

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
      jobArguments: Map[String, Expression] = Map.empty,
      arguments: Map[String, Expression] = Map.empty
    ): InternalExecutable =
      InternalExecutable(
        implicitClass[I].scalaName,
        jobArguments = jobArguments,
        arguments = arguments)

    final def execute(
      agentPath: AgentPath,
      arguments: Map[String, Expression] = Map.empty
    ): Execute.Anonymous =
      Execute(
        WorkflowJob(agentPath, executable(arguments = arguments)))
  }
}
