package com.sos.jobscheduler.core.workflow

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.workflow.OrderContext._
import com.sos.jobscheduler.data.expression.Evaluator.{NumericValue, StringValue, Value}
import com.sos.jobscheduler.data.expression.{Scope, ValueSearch}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
trait OrderContext
{
  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def instruction(workflowPosition: WorkflowPosition): Instruction

  def childOrderEnded(order: Order[Order.State]): Boolean

  protected def idToWorkflow(id: WorkflowId): Checked[Workflow]

  final def makeScope(order: Order[Order.State]): Scope =
    new Scope {
      private lazy val catchCount = Valid(NumericValue(order.workflowPosition.position.catchCount))

      val symbolToValue = {
        case "catchCount" => catchCount
      }

      val findValue = {
        case ValueSearch(ValueSearch.Argument, ValueSearch.KeyValue(name)) =>
          Valid(order.arguments.get(name) map StringValue.apply)

        case ValueSearch(ValueSearch.Argument, ValueSearch.ReturnCode) =>
          Valid(None)

        case ValueSearch(ValueSearch.LastOccurred, ValueSearch.KeyValue(name)) =>
          Valid(
            order.historicOutcomes.reverseIterator
              .collectFirst {
                case HistoricOutcome(_, outcome: Outcome.Undisrupted) if outcome.keyValues.contains(name) =>
                  outcome.keyValues(name)
              }
              .orElse(order.arguments.get(name)) map StringValue.apply)

        case ValueSearch(ValueSearch.LastOccurred, ValueSearch.ReturnCode) =>
          Valid(Some(NumericValue(outcomeToReturnCode(order.lastOutcome).number)))

        case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
          for {
            workflow <- idToWorkflow(order.workflowId)
            maybeValue <- order.historicOutcomes.reverseIterator
              .collectFirst {
                case HistoricOutcome(pos, outcome: Outcome.Undisrupted) if workflow.positionMatchesSearch(pos, positionSearch) =>
                  whatToValue(outcome, what)
              }
              .toChecked(Problem(s"No position in workflow matches '$positionSearch'"))
          } yield maybeValue
      }
    }
}

object OrderContext
{
  private val DisruptedReturnCode = ReturnCode(-1)  // TODO Should we use this value ?

  private def whatToValue(outcome: Outcome.Undisrupted, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.KeyValue(key) => outcome.keyValues.get(key) map StringValue.apply
      case ValueSearch.ReturnCode => Some(NumericValue(outcome.returnCode.number))
    }

  private def outcomeToReturnCode(outcome: Outcome): ReturnCode =
    outcome match {
      case o: Outcome.Undisrupted => o.returnCode
      case _: Outcome.Disrupted => DisruptedReturnCode
    }
}
