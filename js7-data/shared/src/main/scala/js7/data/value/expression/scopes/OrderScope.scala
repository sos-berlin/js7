package js7.data.value.expression.scopes

import js7.data.order.{HistoricOutcome, Order, Outcome}
import js7.data.value.expression.scopes.OrderScope._
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.value.{NumberValue, Value}
import js7.data.workflow.Workflow

final class OrderScope(order: Order[Order.State], workflow: Workflow)
extends Scope
{
  private lazy val catchCount = Right(NumberValue(
    order.workflowPosition.position.catchCount))

  override def symbolToValue(symbol: String) = symbol match {
    case "catchCount" => Some(catchCount)
    case _ => None
  }

  override def findValue(search: ValueSearch) =
    Right(search match {
      case ValueSearch(ValueSearch.Argument, ValueSearch.Name(name)) =>
        argument(name)

      case ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)) =>
        order.historicOutcomes
          .reverseIterator
          .collectFirst {
            case HistoricOutcome(_, outcome: Outcome.Completed)
              if outcome.namedValues.contains(name) =>
              outcome.namedValues(name)
          }
          .orElse(argument(name))

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

  private def argument(name: String): Option[Value] =
    order.arguments.get(name) orElse
      workflow.orderRequirements.defaultArgument(name)
}

object OrderScope
{
  private def whatToValue(outcome: Outcome.Completed, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.Name(key) => outcome.namedValues.get(key)
    }
}
