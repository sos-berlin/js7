package js7.data.value.expression.scopes

import cats.syntax.semigroup.*
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource, JobResourcePath}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.value.expression.Scope.evalLazilyExpressions
import js7.data.value.expression.scopes.OrderScopes.*
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{MissingValue, NumberValue, ObjectValue, StringValue, Value, missingValue}
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Label
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.collection.MapView

/** Provide some Scopes for an Order in any state. */
trait OrderScopes:
  protected val order: Order[Order.State]
  protected val workflow: Workflow
  protected val controllerId: ControllerId

  final lazy val instructionLabel: Option[Label] =
    workflow.labeledInstruction(order.position).toOption.flatMap(_.maybeLabel)

  private def js7VariablesScope =
    minimalJs7VariablesScope(order.id, order.workflowPath, controllerId) |+|
      NamedValueScope:
        case "js7Label" => Right(StringValue:
          instructionLabel.fold("")(_.string))

        case "js7WorkflowPosition" => Right(StringValue:
          order.workflowPosition.toString)

        case "js7TryCount" => Right(NumberValue:
          order.workflowPosition.position.tryCount)

        //case "js7CatchCount" => Right(NumberValue:
        //  order.workflowPosition.position.catchCount)

        case "js7MaxTries" =>
          order.workflowPosition.position.tryPosition.flatMap:
            workflow.instruction_[TryInstruction]
          .map: tryInstruction =>
            tryInstruction.maxTries.fold(missingValue)(NumberValue(_))

  // MUST BE A PURE FUNCTION!
  /** For `Order[Order.State]`, without order variables. */
  final lazy val variablelessOrderScope: Scope =
    combine(
      js7VariablesScope,
      scheduledScope(order.scheduledFor),
      EnvScope)

  /** For `Order[Order.State]`. */
  final lazy val pureOrderScope =
    OrderVariablesScope(order, workflow) |+| variablelessOrderScope


object OrderScopes:
  def apply(order: Order[Order.State], workflow: Workflow, controllerId: ControllerId)
  : OrderScopes =
    val (o, w, id) = (order, workflow, controllerId)
    new OrderScopes:
      protected val order = o
      protected val workflow = w
      protected val controllerId = id

  /** For calculating Workflow.orderVariables. */
  def workflowOrderVariablesScope(
    freshOrder: FreshOrder,
    controllerId: ControllerId,
    pathToJobResource: PartialFunction[JobResourcePath, JobResource],
    nowScope: Scope)
  : Scope =
    val nestedScope = combine(
      scheduledScope(freshOrder.scheduledFor),
      minimalJs7VariablesScope(freshOrder.id, freshOrder.workflowPath, controllerId),
      EnvScope,
      nowScope)
    combine(
      nestedScope,
      NamedValueScope.simple(freshOrder.arguments),
      JobResourceScope(pathToJobResource, useScope = nestedScope))

  def minimalJs7VariablesScope(
    orderId: OrderId,
    workflowPath: WorkflowPath,
    controllerId: ControllerId): Scope
  = NamedValueScope.simple:
    case "js7OrderId" => StringValue(orderId.string)
    case "js7WorkflowPath" => StringValue(workflowPath.string)
    case "js7ControllerId" => StringValue(controllerId.string)

  def scheduledScope(scheduledFor: Option[Timestamp]): Scope =
    TimestampScope("scheduledOrEmpty", scheduledFor)

/** Provide more Scopes for an `Order[Order.Processed]`. */
trait ProcessingOrderScopes extends OrderScopes:
  protected val order: Order[Order.Processing]
  protected val jobKey: JobKey
  protected val workflowJob: WorkflowJob
  protected val jobResources: Seq[JobResource]
  protected val fileValueScope: Scope

  protected[scopes] lazy val nowScope = new NowScope()

  final lazy val simpleJobName: String =
    jobKey match
      case JobKey.Named(_, name) => name.string
      case jobKey => jobKey.name

  /** Number of the execution for this job (starts with 1). */
  final lazy val jobExecutionCount: Int =
    1 + order.historicJobExecutionCount(jobKey, workflow)

  private lazy val js7JobVariablesScope = NamedValueScope.simple:
    case "js7JobName" => StringValue(simpleJobName) // Legacy
    case "js7JobExecutionCount" => NumberValue(jobExecutionCount)
    case "js7Job" => ObjectValue(Map(
      "name" -> StringValue(simpleJobName),
      "sigkillDelayMillis" ->
        workflowJob.sigkillDelay.map(_.toMillis).fold[Value](MissingValue)(NumberValue(_)),
      "timeoutMillis" ->
        workflowJob.timeout.map(_.toMillis).fold[Value](MissingValue)(NumberValue(_)),
      "processLimit" -> NumberValue(workflowJob.processLimit),
      "executionCount" -> NumberValue(jobExecutionCount)))

  /** To avoid name clash, JobResources are not allowed to access order variables. */
  final lazy val scopeForJobResources =
    js7JobVariablesScope |+| variablelessOrderScope |+| nowScope |+| fileValueScope

  private lazy val jobResourceScope = JobResourceScope(
    jobResources.toKeyedMap(_.path),
    useScope = scopeForJobResources)

  final lazy val processingOrderScope =
    js7JobVariablesScope |+| pureOrderScope |+| nowScope |+| jobResourceScope |+| fileValueScope

  /** For Execute defaultArguments. */
  private lazy val scopeForExecuteDefaultArguments =
    js7JobVariablesScope |+| pureOrderScope |+| jobResourceScope

  protected[scopes] final def evalLazilyExecuteDefaultArguments(
    expressionMap: MapView[String, Expression])
  : MapView[String, Checked[Value]] =
    evalLazilyExpressions(expressionMap.view)(scopeForExecuteDefaultArguments)

  /** For WorkflowJob defaultArguments. */
  private lazy val scopeJobDefaultArguments =
    scopeForExecuteDefaultArguments
    // Joacim thinks, without Order variable, it was more predictable for the user:
    // js7JobVariablesScope |+| variablelessOrderScope |+| jobResourceScope

  protected[scopes] final def evalLazilyJobDefaultArguments(
    expressionMap: MapView[String, Expression])
  : MapView[String, Checked[Value]] =
    evalLazilyExpressions(expressionMap.view)(scopeJobDefaultArguments)

  final def evalLazilyJobResourceVariables(jobResource: JobResource)
  : MapView[String, Checked[Value]] =
    evalLazilyExpressions(jobResource.variables.view)(scopeForJobResources)
