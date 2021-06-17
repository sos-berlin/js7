package js7.data.value.expression.scopes

import cats.syntax.semigroup._
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.expression.Scope
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.{Label, Workflow}

/** Provide some Scopes for an Order in any state. */
trait OrderScopes
{
  protected val order: Order[Order.State]
  protected val workflow: Workflow
  protected val controllerId: ControllerId

  final lazy val instructionLabel: Option[Label] =
    workflow.labeledInstruction(order.position).toOption.flatMap(_.maybeLabel)

  private lazy val js7NamesScope: Scope =
    new NamedValueScope(Map(
      "js7OrderId" -> StringValue(order.id.string),
      "js7WorkflowPosition" -> StringValue(order.workflowPosition.toString),
      "js7WorkflowPath" -> StringValue(order.workflowId.path.string),
      "js7Label" -> StringValue(instructionLabel.fold("")(_.string)),
      "js7ControllerId" -> StringValue(controllerId.string)))

  // MUST BE A PURE FUNCTION!
  protected final lazy val scopeWithoutOrderArgumentsOrJob: Scope =
    combine(
      js7NamesScope,
      new TimestampScope("scheduledOrEmpty", order.scheduledFor),
      new OrderSymbolsScope(order),
      EnvScope)

  // MUST BE A PURE FUNCTION!
  final lazy val orderScope: Scope =
    NamedValuesOrderScope(order, workflow) |+| scopeWithoutOrderArgumentsOrJob
}

/** Provide more Scopes for an `Order[Order.Processed]`. */
trait ProcessingOrderScopes extends OrderScopes
{
  protected val order: Order[Order.Processing]
  protected val jobKey: JobKey
  protected val jobResources: Seq[JobResource]

  final lazy val simpleJobName: String =
    jobKey match {
      case JobKey.Named(_, name) => name.string
      case jobKey => jobKey.name
    }

  /** Number of execution for this job (starting with 1). */
  final lazy val jobExecutionCount: Int =
    1 + order.historicJobExecutionCount(jobKey, workflow)

  private lazy val js7JobNameScope: Scope =
    new NamedValueScope(Map(
      "js7JobName" -> StringValue(simpleJobName),
      "js7JobExecutionCount" -> NumberValue(jobExecutionCount)))

  private lazy val jobResourceScope: Scope =
    new JobResourceScope(jobResources.toKeyedMap(_.path).view)

  lazy val nowScope = NowScope()

  /** JobResources are not allowed to access order variables. */
  final lazy val scopeForJobResources: Scope =
    js7JobNameScope |+| scopeWithoutOrderArgumentsOrJob

  final lazy val scopeForEnv: Scope =
    js7JobNameScope |+| scopeWithoutOrderArgumentsOrJob |+| nowScope

  final lazy val scopeForJobDefaultArguments: Scope =
    js7JobNameScope |+| scopeWithoutOrderArgumentsOrJob |+| jobResourceScope

  final lazy val processingOrderScope: Scope =
    js7JobNameScope |+| orderScope |+| jobResourceScope |+| nowScope
}
