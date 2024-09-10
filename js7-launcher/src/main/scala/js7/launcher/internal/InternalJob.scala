package js7.launcher.internal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.charset.Charset
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichPartialFunction}
import js7.data.agent.AgentPath
import js7.data.job.{InternalExecutable, JobConf, JobResourcePath}
import js7.data.order.Order
import js7.data.value.expression.Expression
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.launcher.{OrderProcess, ProcessOrder, StdWriter}
import scala.collection.MapView
import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

trait InternalJob:
  protected type Step = InternalJob.Step

  def start: IO[Checked[Unit]] =
    IO.pure(Right(()))

  def stop: IO[Unit] =
    IO.unit

  def toOrderProcess(step: Step): OrderProcess


object InternalJob:

  final case class JobContext(
    implementationClass: Class[?],
    executable: InternalExecutable,
    jobArguments: NamedValues,
    jobConf: JobConf,
    ioRuntime: IORuntime,
    ioExecutor: IOExecutor,
    blockingJobEC: ExecutionContext,
    clock: AlarmClock,
    systemEncoding: Charset):

    implicit def implicitJs7Scheduler: IORuntime = ioRuntime

  final case class Step private[internal](processOrder: ProcessOrder, arguments: NamedValues):
    self =>

    lazy val jobResourceToVariables: ListMap[JobResourcePath, MapView[String, Checked[Value]]] =
      processOrder.jobResources.view
        .map(jr => jr.path -> processOrder.evalLazilyJobResourceVariables(jr))
        .to(ListMap)

    def env: Checked[Map[String, Option[String]]] =
      processOrder.checkedJobResourcesEnv

    def order: Order[Order.Processing] = processOrder.order
    def workflow: Workflow = processOrder.workflow

    @deprecated("Use write")
    def send(outErr: StdoutOrStderr, string: String): IO[Unit] =
      write(outErr, string).void

    def writeOut(string: String): IO[Boolean] =
      write(Stdout, string)

    def writeErr(string: String): IO[Boolean] =
      write(Stderr, string)

    def write(outErr: StdoutOrStderr, string: String): IO[Boolean] =
      writer(outErr).write(string)

    def writer(outErr: StdoutOrStderr): StdWriter =
      processOrder.stdObservers.writer(outErr)

    def jobResourceVariable(jobResourcePath: JobResourcePath, variableName: String): Checked[Value] =
      jobResourceToVariables
        .rightOr(jobResourcePath, UnknownKeyProblem("JobResource", jobResourcePath.string))
        .flatMap(_.rightOr(variableName, UnknownKeyProblem("JobResource variable", variableName)))
        .flatten

    override def toString = s"Step(${processOrder.order.id} in ${processOrder.jobKey})"

  abstract class Companion[I <: InternalJob](implicit classTag: ClassTag[I]):
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
      subagentBundleId: Option[Expression] = None,
      processLimit: Int = 1,
      timeout: Option[FiniteDuration] = None,
      jobResourcePaths: Seq[JobResourcePath] = Nil,
      isNotRestartable: Boolean = false)
    : Execute.Anonymous =
      Execute(workflowJob(
        agentPath, arguments, subagentBundleId,
        processLimit = processLimit,
        timeout = timeout,
        jobResourcePaths = jobResourcePaths,
        isNotRestartable = isNotRestartable))

    final def workflowJob(
      agentPath: AgentPath,
      arguments: Map[String, Expression] = Map.empty,
      subagentBundleId: Option[Expression] = None,
      processLimit: Int = 1,
      timeout: Option[FiniteDuration] = None,
      jobResourcePaths: Seq[JobResourcePath] = Nil,
      isNotRestartable: Boolean = false)
    : WorkflowJob =
      WorkflowJob(
        agentPath,
        executable(arguments = arguments),
        subagentBundleId = subagentBundleId,
        processLimit = processLimit,
        timeout = timeout,
        jobResourcePaths = jobResourcePaths,
        isNotRestartable = isNotRestartable)
