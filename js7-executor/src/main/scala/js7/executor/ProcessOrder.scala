package js7.executor

import js7.base.utils.CatsUtils.combine
import js7.data.controller.ControllerId
import js7.data.execution.workflow.context.StateView
import js7.data.job.JobKey
import js7.data.order.Order
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.{EnvScope, NamedValueScope, NowScope, OrderScope}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  workflowJob: WorkflowJob,
  jobKey: JobKey,
  defaultArguments: NamedValues,
  controllerId: ControllerId,
  stdObservers: StdObservers)
{
  lazy val jobName: String =
    jobKey match {
      case JobKey.Named(_, name) => name.string
      case _ => jobKey.name
    }

  lazy val jobResourceScope: Scope =
    combine(
      new NamedValueScope(Map(
        "js7OrderId" -> StringValue(order.id.string),
        "js7WorkflowPosition" -> StringValue(order.workflowPosition.toString),
        "js7WorkflowPath" -> StringValue(order.workflowId.path.string),
        "js7Label" -> StringValue(workflow.labeledInstruction(order.position)
          .toOption.flatMap(_.maybeLabel).fold("")(_.string)),
        "js7JobName" -> StringValue(jobName),
        "js7ControllerId" -> StringValue(controllerId.string))),
      NowScope(),
      OrderScope(order),
      EnvScope)

  lazy val scope: Scope =
    combine(
      StateView.makeScope(
        order,
        workflow,
        default = defaultArguments orElse workflowJob.defaultArguments),
      jobResourceScope)
}
