package js7.data.value.expression.scopes

import cats.syntax.semigroup._
import js7.base.problem.Checked
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NumberValue, StringValue, Value}
import js7.data.workflow.{Label, Workflow}
import scala.collection.MapView

/** Provide some Scopes for an Order in any state. */
trait OrderScopes
{
  protected val order: Order[Order.State]
  protected val workflow: Workflow
  protected val controllerId: ControllerId

  final lazy val instructionLabel: Option[Label] =
    workflow.labeledInstruction(order.position).toOption.flatMap(_.maybeLabel)

  private def js7VariablesScope = NamedValueScope {
    case "js7OrderId" => StringValue(order.id.string)
    case "js7WorkflowPosition" => StringValue(order.workflowPosition.toString)
    case "js7WorkflowPath" => StringValue(order.workflowId.path.string)
    case "js7Label" => StringValue(instructionLabel.fold("")(_.string))
    case "js7ControllerId" => StringValue(controllerId.string)
  }

  // MUST BE A PURE FUNCTION!
  /** For `Order[Order.State]`, without order variables. */
  protected final lazy val variablelessOrderScope: Scope =
    combine(
      js7VariablesScope,
      TimestampScope("scheduledOrEmpty", order.scheduledFor),
      SymbolScope {
        case "catchCount" => NumberValue(order.workflowPosition.position.catchCount)
      },
      EnvScope)

  // MUST BE A PURE FUNCTION!
  /** For `Order[Order.State]`. */
  final lazy val orderScope =
    OrderVariablesScope(order, workflow) |+| variablelessOrderScope
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

  private def js7JobVariablesScope = NamedValueScope {
    case "js7JobName" => StringValue(simpleJobName)
    case "js7JobExecutionCount" => NumberValue(jobExecutionCount)
  }

  private[scopes] lazy val nowScope = new NowScope()

  /** To avoid name clash, are JobResources not allowed to access order variables. */
  private lazy val scopeForJobResources =
    js7JobVariablesScope |+| variablelessOrderScope |+| nowScope

  private lazy val jobResourceScope = JobResourceScope(
    jobResources.toKeyedMap(_.path).view,
    useScope = scopeForJobResources)

  final lazy val scopeForJobResourceEnv =
    js7JobVariablesScope |+| nowScope |+| variablelessOrderScope

  final lazy val processingOrderScope =
    js7JobVariablesScope |+| orderScope |+| nowScope |+| jobResourceScope

  private lazy val scopeForJobDefaultArguments =
    js7JobVariablesScope |+| variablelessOrderScope |+| jobResourceScope

  final def evalLazilyJobResourceVariables(jobResource: JobResource): MapView[String, Checked[Value]] =
    scopeForJobResources.evalLazilyExpressionMap(jobResource.variables)

  protected[scopes] final def evalLazilyJobDefaultArguments(expressionMap: Map[String, Expression])
  : MapView[String, Checked[Value]] =
    scopeForJobDefaultArguments.evalLazilyExpressionMap(expressionMap)
}
