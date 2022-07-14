package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichMapView
import js7.data.order.{HistoricOutcome, Order, Outcome}
import js7.data.value.Value
import js7.data.value.expression.scopes.OrderVariablesScope.*
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.workflow.Workflow
import scala.collection.MapView

final class OrderVariablesScope(order: Order[Order.State], workflow: Workflow)
extends Scope
{
  override lazy val nameToCheckedValue =
    orderArguments.mapValues(Right(_))
      .orElseMapView(order
        .historicOutcomes
        .view
        .reverse
        .collect {
          case HistoricOutcome(_, o: Outcome.Completed) =>
            o.namedValues.view.mapValues(Right(_))
        }
        .fold(MapView.empty[String, Checked[Value]])((a, b) => a.orElseMapView(b)))

  override def findValue(search: ValueSearch) =
    search match {
      case ValueSearch(ValueSearch.Argument, ValueSearch.Name(name)) =>
        orderArguments.get(name).map(Right(_))

      case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
        order.historicOutcomes
          .reverseIterator
          .collectFirst {
            case HistoricOutcome(pos, outcome: Outcome.Completed)
              if workflow.positionMatchesSearch(pos, positionSearch) =>
              whatToValue(outcome, what).map(Right(_))
          }
          .flatten

      case _ => super.findValue(search)
    }

  private lazy val orderArguments: MapView[String, Value] =
    workflow.orderParameterList.addDefaults(order.arguments)

  override def toString = s"OrderVariablesScope(${order.id})"
}

object OrderVariablesScope
{
  def apply(order: Order[Order.State], workflow: Workflow): Scope =
    new OrderVariablesScope(order, workflow)

  private def whatToValue(outcome: Outcome.Completed, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.Name(key) => outcome.namedValues.get(key)
    }
}
