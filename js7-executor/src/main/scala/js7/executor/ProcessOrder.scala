package js7.executor

import js7.base.utils.CatsUtils.combine
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.{EnvScope, JobResourceScope, NamedValueScope, NowScope, OrderScheduledScope, OrderScope}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.{Label, Workflow}

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  jobKey: JobKey,
  jobResources: Seq[JobResource],
  defaultArguments: NamedValues,
  controllerId: ControllerId,
  stdObservers: StdObservers)
{
  lazy val simpleJobName: String =
    jobKey match {
      case JobKey.Named(_, name) => name.string
      case _ => jobKey.name
    }

  lazy val instructionLabel: Option[Label] = workflow.labeledInstruction(order.position)
    .toOption.flatMap(_.maybeLabel)

  private lazy val js7NamesScope: Scope =
    new NamedValueScope(Map(
      "js7OrderId" -> StringValue(order.id.string),
      "js7WorkflowPosition" -> StringValue(order.workflowPosition.toString),
      "js7WorkflowPath" -> StringValue(order.workflowId.path.string),
      "js7JobName" -> StringValue(simpleJobName),
      "js7JobExecutionCount" -> NumberValue(jobExecutionCount),
      "js7Label" -> StringValue(instructionLabel.fold("")(_.string)),
      "js7ControllerId" -> StringValue(controllerId.string)))

  lazy val scopeForJobResource: Scope =
    combine(EnvScope, new NowScope, js7NamesScope, new OrderScheduledScope(order))

  lazy val scope: Scope =
    combine(
      new OrderScope(order, workflow),
      new NamedValueScope(defaultArguments),
      new JobResourceScope(jobResources),
      scopeForJobResource)

  /** Number of execution for this job (starting with 1). */
  lazy val jobExecutionCount: Int =
    1 + order.historicJobExecutionCount(jobKey, workflow)
}
