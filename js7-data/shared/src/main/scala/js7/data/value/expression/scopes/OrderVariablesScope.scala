package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.order.{HistoricOutcome, Order, OrderOutcome}
import js7.data.value.Value
import js7.data.value.expression.scopes.OrderVariablesScope.*
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.workflow.Workflow
import scala.collection.MapView

final class OrderVariablesScope(order: Order[Order.State], workflow: Workflow)
extends Scope:

  override def namedValue(name: String): Option[Checked[Value]] =
    workflow.orderParameterList.getArgument(name, order.arguments).orElse:
      order.historicOutcomes.view.reverse.flatMap:
        case HistoricOutcome(_, o: OrderOutcome) =>
          o.findNamedValues.flatMap(_.get(name))
      .headOption
    .map(Right(_))

  override def findValue(search: ValueSearch): Option[Checked[Value]] =
    search match
      case ValueSearch(ValueSearch.Argument, ValueSearch.Name(name)) =>
        workflow.orderParameterList.getArgument(name, order.arguments)
          .map(Right(_))

      case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
        order.historicOutcomes
          .reverseIterator
          .collectFirst:
            case HistoricOutcome(pos, outcome: OrderOutcome)
              if workflow.positionMatchesSearch(pos, positionSearch) =>
              whatToValue(outcome, what).map(Right(_))
          .flatten

      case _ => super.findValue(search)

  private lazy val orderArguments: MapView[String, Value] =
    workflow.orderParameterList.addDefaults(order.arguments)

  override def toString = s"OrderVariablesScope(${order.id})"


object OrderVariablesScope:

  def apply(order: Order[Order.State], workflow: Workflow): Scope =
    new OrderVariablesScope(order, workflow)

  private def whatToValue(outcome: OrderOutcome, what: ValueSearch.What): Option[Value] =
    what match
      case ValueSearch.Name(key) => outcome.findNamedValues.flatMap(_.get(key))
