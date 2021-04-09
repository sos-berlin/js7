package js7.executor.internal

import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.monixutils.ObserverAsTask
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.data.order.Order
import js7.data.order.Order.Processing
import js7.data.value.expression.Scope
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.Workflow
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observer

trait InternalJob
{
  def start: Task[Checked[Unit]] =
    Task.pure(Right(()))

  def stop: Task[Unit] =
    Task.unit

  def processOrder(orderContext: OrderContext): OrderProcess
}

object InternalJob
{
  final case class JobContext(
    implementationClass: Class[_],
    jobArguments: Map[String, Value],
    implicit val js7Scheduler: Scheduler,
    ioExecutor: IOExecutor,
    blockingJobScheduler: Scheduler)

  final case class OrderContext private(
    arguments: NamedValues,
    order: Order[Processing],
    workflow: Workflow,
    scope: Scope,
    outObserver: Observer[String],
    errObserver: Observer[String])
  { self =>
    private val outErrToObserverAsTask = Map(
      Stdout -> ObserverAsTask(outObserver),
      Stderr -> ObserverAsTask(errObserver))

    def send(outErr: StdoutOrStderr, string: String): Task[Unit] =
      outErrToObserverAsTask(outErr).sendOrRaise(string)

    def outErrObserver(stdoutOrStderr: StdoutOrStderr): Observer[String] =
      stdoutOrStderr match {
        case Stdout => outObserver
        case Stderr => errObserver
      }
  }
}
