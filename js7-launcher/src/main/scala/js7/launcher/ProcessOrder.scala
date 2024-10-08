package js7.launcher

import cats.effect.ResourceIO
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.value.expression.scopes.{FileValueScope, FileValueState, NamedValueScope, ProcessingOrderScopes}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{MissingValue, Value}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.launcher.ProcessOrder.evalEnv
import scala.collection.MapView

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  jobResources: Seq[JobResource],
  executeArguments: Map[String, Expression],
  jobArguments: Map[String, Expression],
  controllerId: ControllerId,
  stdObservers: StdObservers,
  fileValueScope: Scope)
extends ProcessingOrderScopes:
  /** Lazily evaluated defaultArguments. */
  private lazy val nameToLazyDefaultArgument: MapView[String, Checked[Value]] =
    evalLazilyJobDefaultArguments(jobArguments.view) +++
      evalLazilyExecuteDefaultArguments(executeArguments.view) // Execute overrides WorkflowJob

  lazy val checkedJobResourcesEnv: Checked[Map[String, Option[String]]] =
    jobResources
      .reverse/*left overrides right*/
      .traverse(evalJobResourceEnv)
      .map(_.fold(Map.empty)(_ ++ _))

  private def evalJobResourceEnv(jobResource: JobResource): Checked[Map[String, Option[String]]] =
    evalEnv(
      jobResource.env,
      scopeForJobResources |+|
        NamedValueScope(evalLazilyJobResourceVariables(jobResource)))

  /** Eagerly evaluated defaultArguments, used for JS1 compatibility. */
  lazy val checkedJs1DefaultArguments: Checked[Map[String, Value]] =
    nameToLazyDefaultArgument
      .toVector
      .traverse { case (k, checked) => checked.map(k -> _) }
      .map(_.toMap)

  lazy val scope: Scope =
    processingOrderScope |+|
      NamedValueScope(nameToLazyDefaultArgument)


object ProcessOrder:
  def resource(
    order: Order[Order.Processing],
    workflow: Workflow,
    jobKey: JobKey,
    workflowJob: WorkflowJob,
    jobResources: Seq[JobResource],
    executeArguments: Map[String, Expression],
    jobArguments: Map[String, Expression],
    controllerId: ControllerId,
    stdObservers: StdObservers,
    fileValueState: FileValueState)
  : ResourceIO[ProcessOrder] =
    for fileValueScope <- FileValueScope.resource(fileValueState) yield
      ProcessOrder(
        order, workflow, jobKey, workflowJob, jobResources,
        executeArguments, jobArguments, controllerId, stdObservers,
        fileValueScope)

  def evalEnv(nameToExpr: Map[String, Expression], scope: => Scope)
  : Checked[Map[String, Option[String]]] =
    evalExpressionMap(nameToExpr, scope)
      .flatMap(_
        .view
        .mapValues {
          case MissingValue => None
          case v => Some(v)
        }
        .toVector
        .traverse {
          case (k, v) => v.traverse(_.toStringValueString).map(k -> _)
        })
      .map(_.toMap)
