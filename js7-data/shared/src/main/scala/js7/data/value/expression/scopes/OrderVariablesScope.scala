package js7.data.value.expression.scopes

import js7.data.order.{HistoricOutcome, Order, Outcome}
import js7.data.value.Value
import js7.data.value.expression.scopes.OrderVariablesScope._
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.workflow.Workflow

final class OrderVariablesScope(order: Order[Order.State], workflow: Workflow)
extends Scope
{
  override def findValue(search: ValueSearch)(implicit scope: Scope) =
    Right(search match {
      case ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)) =>
        argument(name)
          .orElse(order
            .historicOutcomes.view
            .reverse
            .flatMap {
              case HistoricOutcome(_, o: Outcome.Completed) => o.namedValues.get(name)
              case _ => None
            }.headOption)

      case ValueSearch(ValueSearch.Argument, ValueSearch.Name(name)) =>
        argument(name)

      case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
        order.historicOutcomes
          .reverseIterator
          .collectFirst {
            case HistoricOutcome(pos, outcome: Outcome.Completed)
              if workflow.positionMatchesSearch(pos, positionSearch) =>
              whatToValue(outcome, what)
          }
          .flatten
    })

  private def argument(name: String)(implicit scope: Scope): Option[Value] =
    order.arguments.get(name) orElse
      workflow.orderRequirements.defaultArgument(name)

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
