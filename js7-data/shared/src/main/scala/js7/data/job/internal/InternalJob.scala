package js7.data.job.internal

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.checkedCast
import js7.data.job.internal.InternalJob._
import js7.data.order.Order
import js7.data.value.expression.{Evaluator, ExpressionParser, Scope, ValueSearch}
import js7.data.value.{NamedValues, NumberValue, Value}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import monix.eval.Task

/** Internal Job with internal interface — not for public use.
  *
  * This job runs in JS7's JVM, so be careful!
  *
  * The contructor of the implementing class must have a `JobContext` parameter.
  */
trait InternalJob
{
  def processOrder(orderProcessingContext: OrderContext): Task[Checked[NamedValues]]

  //def cancelAll(): Unit
  //def cancel(orderId: OrderId): Unit
  //def stop: Task[Unit]
}

object InternalJob
{
  final case class JobContext(
    workflowJob: WorkflowJob,
    workflow: Workflow)

  final case class OrderContext(
    order: Order[Order.Processing],
    scope: Scope,
    arguments: NamedValues)
    //cancel: () => Unit
  {
    def variable(name: String): Checked[Option[Value]] =
      scope.findValue(ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)))

    def evalToBigDecimal(string: String): Checked[BigDecimal] =
      eval(string)
        .flatMap(checkedCast[NumberValue])
        .map(_.number)

    def eval(string: String): Checked[Value] =
      ExpressionParser.parse(string)
        .flatMap(new Evaluator(scope).eval)
  }
}
