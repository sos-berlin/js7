package js7.launcher

import cats.effect.Resource
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.value.expression.scopes.{FileValueScope, FileValueState, NameToCheckedValueScope, ProcessingOrderScopes}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NullValue, Value}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.launcher.ProcessOrder.evalEnv
import monix.eval.Task
import scala.collection.MapView

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  jobResources: Seq[JobResource],
  defaultArgumentExpressions: Map[String, Expression],
  controllerId: ControllerId,
  stdObservers: StdObservers,
  fileValueScope: Scope)
extends ProcessingOrderScopes
{
  /** Lazily evaluated defaultArguments. */
  private lazy val nameToLazyDefaultArgument: MapView[String, Checked[Value]] =
    evalLazilyJobDefaultArguments(defaultArgumentExpressions.view)

  lazy val checkedJobResourcesEnv: Checked[Map[String, String]] =
    jobResources
      .reverse/*left overrides right*/
      .traverse(evalJobResourceEnv)
      .map(_.fold(Map.empty)(_ ++ _))

  private def evalJobResourceEnv(jobResource: JobResource): Checked[Map[String, String]] =
    evalEnv(
      jobResource.env,
      scopeForJobResources |+|
        NameToCheckedValueScope(evalLazilyJobResourceVariables(jobResource)))

  /** Eagerly evaluated defaultArguments, used for JS1 compatibility. */
  lazy val checkedDefaultArguments: Checked[Map[String, Value]] =
    nameToLazyDefaultArgument
      .toVector
      .traverse { case (k, checked) => checked.map(k -> _) }
      .map(_.toMap)

  lazy val scope: Scope =
    processingOrderScope |+|
      NameToCheckedValueScope(nameToLazyDefaultArgument)
}

object ProcessOrder
{
  def resource(
    order: Order[Order.Processing],
    workflow: Workflow,
    jobKey: JobKey,
    workflowJob: WorkflowJob,
    jobResources: Seq[JobResource],
    defaultArgumentExpressions: Map[String, Expression],
    controllerId: ControllerId,
    stdObservers: StdObservers,
    fileValueState: FileValueState)
  : Resource[Task, ProcessOrder] =
    for (fileValueScope <- FileValueScope.resource(fileValueState)) yield
      ProcessOrder(
        order, workflow, jobKey, workflowJob, jobResources,
        defaultArgumentExpressions, controllerId, stdObservers,
        fileValueScope)

  def evalEnv(nameToExpr: Map[String, Expression], scope: => Scope)
  : Checked[Map[String, String]] =
    evalExpressionMap(nameToExpr, scope)
      .flatMap(_
        .view
        .filter(_._2 != NullValue)  // TODO Experimental
        .toVector.traverse { case (k, v) => v.toStringValueString.map(k -> _) })
      .map(_.toMap)
}
