package js7.executor.internal

import js7.base.problem.Checked
import js7.data.job.InternalExecutable
import js7.data.order.Order
import js7.data.value.NamedValues
import js7.data.value.expression.Scope
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.internal.InternalJob._
import monix.eval.Task

trait InternalJob
{
  def start: Task[Checked[Unit]] =
    Task(Right(()))

  def processOrder(context: OrderContext): OrderProcess
}

object InternalJob
{
  final case class JobContext(
    executable: InternalExecutable,
    workflowJob: WorkflowJob)

  final case class OrderContext private(
    order: Order[Order.Processing],
    workflow: Workflow,
    arguments: NamedValues,
    scope: Scope)

  final case class OrderProcess private(
    completed: Task[Checked[Result]])
    //cancel: ProcessSignal => Unit = _ => ())

  final case class Result private(namedValues: NamedValues)
}
