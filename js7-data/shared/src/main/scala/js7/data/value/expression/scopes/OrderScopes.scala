package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource, JobResourcePath}
import js7.data.order.{FreshOrder, Order, OrderDetails, OrderId}
import js7.data.value.expression.Scope.evalLazilyExpressions
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{BooleanValue, MissingValue, NumberValue, ObjectValue, StringValue, Value}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.TryInstruction
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Label
import org.jetbrains.annotations.TestOnly
import scala.collection.MapView
import scala.collection.immutable.Map.Map1

/** Provide some Scopes for an Order in any state. */
trait OrderScopes:

  protected val order: Order[Order.State]
  protected val workflow: Workflow
  protected val controllerId: ControllerId

  final lazy val instructionLabel: Option[Label] =
    workflow.labeledInstruction(order.position).toOption.flatMap(_.maybeLabel)

  private lazy val symbolScope =
    ArgumentlessFunctionScope:
      case "tryCount" => Right(NumberValue:
        order.workflowPosition.position.tryCount)

      //case "catchCount" => NumberValue:
      //  order.workflowPosition.position.catchCount

      case "maxTries" =>
        order.workflowPosition.position.tryPosition.flatMap:
          workflow.instruction_[TryInstruction]
        .map: tryInstruction =>
          tryInstruction.maxTries.fold(MissingValue)(NumberValue(_))

      case "timedOut" => Right(BooleanValue:
        order.hasTimedOut)

      case "label" => Right(StringValue:
        instructionLabel.fold("")(_.string))

      case "workflowPosition" => Right(StringValue:
        order.workflowPosition.toString)

      case "workflow" => Right(ObjectValue(Map1(
        "timezone", StringValue(workflow.timeZone.string))))

  // MUST BE A PURE FUNCTION!
  /** For `Order[Order.State]`, without order variables. */
  final lazy val variablelessOrderScope: Scope =
    combine(
      order.minimumScope(controllerId),
      symbolScope,
      NamedValueScope:
        case "js7Label" => symbolScope.symbol("label").get
        case "js7WorkflowPosition" => symbolScope.symbol("workflowPosition").get
        case "js7TryCount" => symbolScope.symbol("tryCount").get
        case "js7MaxTries" => symbolScope.symbol("maxTries").get
        case "js7WorkflowTimezone" => Right(StringValue(workflow.timeZone.string)),
      EnvScope)

  /** For `Order[Order.State]`. */
  final lazy val pureOrderScope: Scope =
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
  @TestOnly // ???
  def workflowOrderVariablesScope(
    freshOrder: FreshOrder,
    controllerId: ControllerId,
    pathToJobResource: PartialFunction[JobResourcePath, JobResource],
    nowScope: Scope)
  : Scope =
    val nestedScope = combine(
      freshOrder.minimumScope(controllerId),
      EnvScope,
      nowScope)
    combine(
      nestedScope,
      NamedValueScope.simple(freshOrder.arguments),
      JobResourceScope(pathToJobResource, useScope = nestedScope))

  /** A Scope that does not change in the Order's lifetime. */
  def minimumOrderScope(orderId: OrderId, orderDetails: OrderDetails, controllerId: ControllerId)
  : Scope =
    val symbolScope = ArgumentlessFunctionScope.simpleJava:
      case "orderId" => orderId.string
      case "controllerId" => controllerId.string
      // Not sure about workflowPath with future callable Workflows ???
      // This must be constant. In a future nested workflow, this must be the original workflow.
      case "workflowPath" => orderDetails.workflowPath.string
    combine(
      symbolScope,
      NamedValueScope:
        case "js7OrderId" => symbolScope.symbol("orderId").get
        case "js7ControllerId" => symbolScope.symbol("controllerId").get
        case "js7WorkflowPath" => symbolScope.symbol("workflowPath").get,
      TimestampScope("scheduledOrEmpty", orderDetails.scheduledFor))

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

  private lazy val js7JobVariablesScope = NamedValueScope.simpleJava:
    case "js7JobName" => simpleJobName // Legacy
    case "js7JobExecutionCount" => jobExecutionCount
    case "js7Job" => ObjectValue.unsafeSimpleJava(
      "name" -> simpleJobName,
      "sigkillDelayMillis" ->
        workflowJob.sigkillDelay.map(_.toMillis).fold[Value](MissingValue)(NumberValue(_)),
      "timeoutMillis" ->
        workflowJob.timeout.map(_.toMillis).fold[Value](MissingValue)(NumberValue(_)),
      "processLimit" -> workflowJob.processLimit,
      "executionCount" -> jobExecutionCount)

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
