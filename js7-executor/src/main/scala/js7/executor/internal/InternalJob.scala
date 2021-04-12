package js7.executor.internal

import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.monixutils.TaskObserver
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.data.order.Order
import js7.data.order.Order.Processing
import js7.data.value.expression.Scope
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.Workflow
import js7.executor.{OrderProcess, StdObservers}
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer

trait InternalJob
{
  protected type Step = InternalJob.Step

  def start: Task[Checked[Unit]] =
    Task.pure(Right(()))

  def stop: Task[Unit] =
    Task.unit

  def processOrder(step: Step): OrderProcess
}

object InternalJob
{
  final case class JobContext(
    implementationClass: Class[_],
    jobArguments: Map[String, Value],
    implicit val js7Scheduler: Scheduler,
    ioExecutor: IOExecutor,
    blockingJobScheduler: Scheduler)

  final case class Step private(
    arguments: NamedValues,
    order: Order[Processing],
    workflow: Workflow,
    scope: Scope,
    stdObservers: StdObservers)
  { self =>
    def outObserver = stdObservers.out
    def errObserver = stdObservers.err
    def outTaskObserver = stdObservers.outTaskObserver
    def errTaskObserver = stdObservers.errTaskObserver

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
  }
}
