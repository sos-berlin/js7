package js7.launcher

import cats.effect.Resource
import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.Value
import js7.data.value.expression.scopes.{FileValueScope, FileValueState, NameToCheckedValueScope, ProcessingOrderScopes}
import js7.data.value.expression.{Expression, Scope}
import js7.data.workflow.Workflow
import monix.eval.Task
import scala.collection.MapView

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  jobKey: JobKey,
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

  /** Eagerly evaluated defaultArguments. */
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
    jobResources: Seq[JobResource],
    defaultArgumentExpressions: Map[String, Expression],
    controllerId: ControllerId,
    stdObservers: StdObservers,
    fileValueState: FileValueState)
  : Resource[Task, ProcessOrder] =
    for (fileValueScope <- FileValueScope.resource(fileValueState)) yield
      ProcessOrder(
        order, workflow, jobKey, jobResources,
        defaultArgumentExpressions, controllerId, stdObservers,
        fileValueScope)
}
