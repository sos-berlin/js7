package js7.executor

import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order
import js7.data.value.Value
import js7.data.value.expression.scopes.{LazyNamedValueScope, ProcessingOrderScopes}
import js7.data.value.expression.{Expression, Scope}
import js7.data.workflow.Workflow
import scala.collection.MapView

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  jobKey: JobKey,
  jobResources: Seq[JobResource],
  defaultArgumentExpressions: Map[String, Expression],
  controllerId: ControllerId,
  stdObservers: StdObservers)
extends ProcessingOrderScopes
{
  /** Lazily evaluated defaultArguments. */
  private lazy val nameToLazyDefaultArgument: MapView[String, Checked[Value]] =
    evalLazilyJobDefaultArguments(defaultArgumentExpressions)

  /** Eagerly evaluated defaultArguments. */
  lazy val checkedDefaultArguments: Checked[Map[String, Value]] =
    nameToLazyDefaultArgument
      .toVector
      .traverse { case (k, checked) => checked.map(k -> _) }
      .map(_.toMap)

  lazy val scope: Scope =
    processingOrderScope |+| LazyNamedValueScope(nameToLazyDefaultArgument)
}
