package js7.executor

import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.value.NamedValues
import js7.data.value.expression.Scope
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  workflowJob: WorkflowJob,
  defaultArguments: NamedValues,
  stdObservers: StdObservers)
{
  lazy val scope: Scope =
    StateView.makeScope(
      order,
      workflow,
      default = defaultArguments orElse workflowJob.defaultArguments)

  lazy val evaluator = scope.evaluator
}
