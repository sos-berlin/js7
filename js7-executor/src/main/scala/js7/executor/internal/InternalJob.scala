package js7.executor.internal

import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.monixutils.TaskObserver
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.data.job.{InternalExecutable, JobConf}
import js7.data.value.NamedValues
import js7.executor.{OrderProcess, ProcessOrder}
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

  def toOrderProcess(step: Step): OrderProcess
}

object InternalJob
{
  final case class JobContext(
    implementationClass: Class[_],
    executable: InternalExecutable,
    jobConf: JobConf,
    implicit val js7Scheduler: Scheduler,
    ioExecutor: IOExecutor,
    blockingJobScheduler: Scheduler)

  final case class Step private(
    arguments: NamedValues,
    processOrder: ProcessOrder)
  { self =>
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
  }
}
