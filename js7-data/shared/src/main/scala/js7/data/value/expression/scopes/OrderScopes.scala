package js7.data.value.expression.scopes

import cats.syntax.semigroup._
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource, JobResourcePath}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.value.expression.scopes.OrderScopes._
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NumberValue, StringValue, Value}
import js7.data.workflow.{Label, Workflow, WorkflowPath}
import scala.collection.MapView

/** Provide some Scopes for an Order in any state. */
trait OrderScopes
{
  protected val order: Order[Order.State]
  protected val workflow: Workflow
  protected val controllerId: ControllerId

  final lazy val instructionLabel: Option[Label] =
    workflow.labeledInstruction(order.position).toOption.flatMap(_.maybeLabel)

  private def js7VariablesScope =
    minimalJs7VariablesScope(order.id, order.workflowId.path, controllerId) |+|
      NamedValueScope(Map(
        "js7Label" -> StringValue(instructionLabel.fold("")(_.string)),
        "js7WorkflowPosition" -> StringValue(order.workflowPosition.toString)))

  // MUST BE A PURE FUNCTION!
  /** For `Order[Order.State]`, without order variables. */
  protected final lazy val variablelessOrderScope: Scope =
    combine(
      js7VariablesScope,
      scheduledScope(order.scheduledFor),
      SymbolScope {
        case "catchCount" => NumberValue(order.workflowPosition.position.catchCount)
      },
      EnvScope)

  // MUST BE A PURE FUNCTION!
  /** For `Order[Order.State]`. */
  final lazy val orderScope =
    OrderVariablesScope(order, workflow) |+| variablelessOrderScope

  protected[scopes] lazy val nowScope = new NowScope()
}

object OrderScopes
{
  def apply(order: Order[Order.State], workflow: Workflow, controllerId: ControllerId)
  : OrderScopes = {
    val (o, w, id) = (order, workflow, controllerId)
    new OrderScopes {
      protected val order = o
      protected val workflow = w
      protected val controllerId = id
    }
  }

  /** For calculating Workflow.orderVariables. */
  def workflowOrderVariablesScope(
    freshOrder: FreshOrder,
    pathToJobResource: PartialFunction[JobResourcePath, JobResource],
    controllerId: ControllerId,
    nowScope: Scope)
  : Scope = {
    val nestedScope = combine(
      scheduledScope(freshOrder.scheduledFor),
      minimalJs7VariablesScope(freshOrder.id, freshOrder.workflowPath, controllerId),
      EnvScope,
      nowScope)
    combine(
      nestedScope,
      NamedValueScope(freshOrder.arguments),
      JobResourceScope(pathToJobResource, useScope = nestedScope))
  }

  private def minimalJs7VariablesScope(
    orderId: OrderId,
    workflowPath: WorkflowPath,
    controllerId: ControllerId)
  = NamedValueScope(Map(
     "js7OrderId" -> StringValue(orderId.string),
     "js7WorkflowPath" -> StringValue(workflowPath.string),
     "js7ControllerId" -> StringValue(controllerId.string)))

  private def scheduledScope(scheduledFor: Option[Timestamp]) =
    TimestampScope("scheduledOrEmpty", scheduledFor)
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

  /** Number of the execution for this job (starts with 1). */
  final lazy val jobExecutionCount: Int =
    1 + order.historicJobExecutionCount(jobKey, workflow)

  private def js7JobVariablesScope = NamedValueScope(Map(
    "js7JobName" -> StringValue(simpleJobName),
    "js7JobExecutionCount" -> NumberValue(jobExecutionCount)))

  /** To avoid name clash, JobResources are not allowed to access order variables. */
  private lazy val scopeForJobResources =
    js7JobVariablesScope |+| variablelessOrderScope |+| nowScope

  private lazy val jobResourceScope = JobResourceScope(
    jobResources.toKeyedMap(_.path),
    useScope = scopeForJobResources)

  final lazy val scopeForJobResourceEnv =
    js7JobVariablesScope |+| variablelessOrderScope |+| nowScope

  final lazy val processingOrderScope =
    js7JobVariablesScope |+| orderScope |+| nowScope |+| jobResourceScope

  /** For defaultArguments (Execute and WorkflowJob). */
  private lazy val scopeForOrderDefaultArguments =
    js7JobVariablesScope |+| variablelessOrderScope |+| jobResourceScope

  final def evalLazilyJobResourceVariables(jobResource: JobResource): MapView[String, Checked[Value]] =
    scopeForJobResources.evalLazilyExpressionMap(jobResource.variables)

  protected[scopes] final def evalLazilyJobDefaultArguments(expressionMap: Map[String, Expression])
  : MapView[String, Checked[Value]] =
    scopeForOrderDefaultArguments.evalLazilyExpressionMap(expressionMap)
}
